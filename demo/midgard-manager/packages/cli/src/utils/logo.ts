import chalk from "chalk";

/**
 * Logo display options
 */
export interface LogoOptions {
  /** Show full or condensed version */
  variant?: "full" | "small";
  /** Show header text after the logo */
  headerText?: string;
}

/**
 * Displays the Midgard logo with configurable options
 */
export function displayLogo(options: LogoOptions = {}): void {
  const { variant = "full", headerText } = options;
  const treeEmoji = "🌳";

  if (variant === "full") {
    console.log("\n");
    console.log("┌─ MIDGARD CLI");
    console.log(
      `│  ${treeEmoji}  ${chalk.bold.green("MIDGARD")}  ${chalk.dim(
        "CLI PLATFORM"
      )}`
    );
    console.log("│  A toolkit for Cardano development");
    console.log("└────────────");
  } else {
    // Small variant
    console.log(`┌─ ${treeEmoji} ${chalk.bold.green("MIDGARD MANAGER")} `);
    console.log(`│ ${chalk.dim("MVP")} · CLI Tool for Testing/Demo`);
    console.log("└────────────");
  }

  // Show header text if provided
  if (headerText) {
    console.log(`\n${chalk.bold(headerText)}\n`);
  }

  // Only add extra newline for full logo
  if (variant === "full") {
    console.log("\n");
  }
}

/**
 * @deprecated Use displayLogo({ variant: 'small' }) instead
 */
export function displaySmallLogo(): void {
  displayLogo({ variant: "small" });
}
