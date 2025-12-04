import { chromium } from 'playwright';
import { existsSync, mkdirSync, writeFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import { createAuthenticatedContext, hasAuthState } from './session.js';

const __dirname = dirname(fileURLToPath(import.meta.url));
const PROJECT_ROOT = join(__dirname, '..');

function getAocBase(year) {
  return `https://adventofcode.com/${year}`;
}

function getDayDir(year, day) {
  const dayStr = String(day).padStart(2, '0');
  return join(PROJECT_ROOT, String(year), `day${dayStr}`);
}

function ensureDayStructure(year, day) {
  const dayDir = getDayDir(year, day);
  const nodeDir = join(dayDir, 'node');
  const pythonDir = join(dayDir, 'python');

  if (!existsSync(dayDir)) mkdirSync(dayDir, { recursive: true });
  if (!existsSync(nodeDir)) mkdirSync(nodeDir);
  if (!existsSync(pythonDir)) mkdirSync(pythonDir);

  // Create stub solution files if they don't exist
  const nodeSolution = join(nodeDir, 'solution.js');
  const pythonSolution = join(pythonDir, 'solution.py');
  const nodePackage = join(nodeDir, 'package.json');

  if (!existsSync(nodeSolution)) {
    writeFileSync(nodeSolution, `import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input
const lines = input.split('\\n');

// Part 1
function part1() {
  // TODO: Implement
  return null;
}

// Part 2
function part2() {
  // TODO: Implement
  return null;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
`);
  }

  if (!existsSync(pythonSolution)) {
    writeFileSync(pythonSolution, `from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input
lines = input_text.split("\\n")


def part1():
    # TODO: Implement
    return None


def part2():
    # TODO: Implement
    return None


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
`);
  }

  if (!existsSync(nodePackage)) {
    writeFileSync(nodePackage, JSON.stringify({ type: 'module' }, null, 2));
  }

  return dayDir;
}

async function extractProblem(page, year, day) {
  const url = `${getAocBase(year)}/day/${day}`;
  console.log(`Fetching problem from ${url}...`);

  await page.goto(url);

  // Extract all article elements (problem descriptions)
  const articles = await page.locator('article.day-desc').all();

  if (articles.length === 0) {
    console.log('No problem found. Day may not be available yet.');
    return null;
  }

  let markdown = '';

  for (let i = 0; i < articles.length; i++) {
    const article = articles[i];
    const html = await article.innerHTML();

    // Convert HTML to markdown-ish format
    let text = html
      // Headers
      .replace(/<h2[^>]*>--- (.*?) ---<\/h2>/g, '# $1\n\n')
      // Code blocks
      .replace(/<pre><code>([\s\S]*?)<\/code><\/pre>/g, '```\n$1\n```\n')
      // Inline code
      .replace(/<code><em>(.*?)<\/em><\/code>/g, '`**$1**`')
      .replace(/<code>(.*?)<\/code>/g, '`$1`')
      // Emphasis
      .replace(/<em class="star">([^<]*)<\/em>/g, '**$1**')
      .replace(/<em>(.*?)<\/em>/g, '**$1**')
      // Lists
      .replace(/<ul>([\s\S]*?)<\/ul>/g, (match, content) => {
        return content.replace(/<li>([\s\S]*?)<\/li>/g, '- $1\n');
      })
      // Paragraphs
      .replace(/<p>([\s\S]*?)<\/p>/g, '$1\n\n')
      // Links
      .replace(/<a[^>]*href="([^"]*)"[^>]*>(.*?)<\/a>/g, '[$2]($1)')
      // Spans
      .replace(/<span[^>]*>(.*?)<\/span>/g, '$1')
      // Clean up remaining tags
      .replace(/<\/?[^>]+(>|$)/g, '')
      // Decode entities
      .replace(/&lt;/g, '<')
      .replace(/&gt;/g, '>')
      .replace(/&amp;/g, '&')
      .replace(/&quot;/g, '"')
      // Clean up whitespace
      .replace(/\n{3,}/g, '\n\n')
      .trim();

    markdown += text + '\n\n';
  }

  return markdown.trim();
}

async function extractInput(page, year, day) {
  const url = `${getAocBase(year)}/day/${day}/input`;
  console.log(`Fetching input from ${url}...`);

  await page.goto(url);

  // The input page is just raw text in a <pre> tag
  const input = await page.locator('pre').textContent();

  return input;
}

async function extractDay(year, day) {
  if (!hasAuthState()) {
    console.error('No saved session. Run "node session.js login" first.');
    process.exit(1);
  }

  console.log(`\n=== Extracting Year ${year} Day ${day} ===\n`);

  const browser = await chromium.launch({ headless: true });
  const context = await createAuthenticatedContext(browser);
  const page = await context.newPage();

  try {
    // Extract problem statement
    const problem = await extractProblem(page, year, day);
    if (!problem) {
      await browser.close();
      return;
    }

    // Extract input
    const input = await extractInput(page, year, day);

    // Set up directory structure and save files
    const dayDir = ensureDayStructure(year, day);

    const problemPath = join(dayDir, 'problem.md');
    const inputPath = join(dayDir, 'input.txt');

    writeFileSync(problemPath, problem);
    console.log(`Problem saved to ${problemPath}`);

    writeFileSync(inputPath, input);
    console.log(`Input saved to ${inputPath}`);

    console.log(`\nDay ${day} extracted successfully!`);
    console.log(`  - Problem: ${problemPath}`);
    console.log(`  - Input: ${inputPath}`);
    console.log(`  - Node solution: ${join(dayDir, 'node', 'solution.js')}`);
    console.log(`  - Python solution: ${join(dayDir, 'python', 'solution.py')}`);

  } finally {
    await browser.close();
  }
}

// CLI handling
const args = process.argv.slice(2);
let day = null;
let year = new Date().getFullYear(); // Default to current year

for (let i = 0; i < args.length; i++) {
  if (args[i] === '--day' || args[i] === '-d') {
    day = parseInt(args[i + 1], 10);
    i++;
  } else if (args[i] === '--year' || args[i] === '-y') {
    year = parseInt(args[i + 1], 10);
    i++;
  } else {
    // Support positional: year day (e.g., "2024 1")
    const num = parseInt(args[i], 10);
    if (!isNaN(num)) {
      if (num > 2000) {
        year = num;
      } else {
        day = num;
      }
    }
  }
}

if (!day || day < 1 || day > 25) {
  console.log('Usage: node extract.js --year <year> --day <1-25>');
  console.log('   or: node extract.js <year> <day>');
  console.log('   or: node extract.js <day>  (uses current year)');
  process.exit(1);
}

if (year < 2015 || year > new Date().getFullYear()) {
  console.log(`Invalid year: ${year}. AoC started in 2015.`);
  process.exit(1);
}

await extractDay(year, day);
