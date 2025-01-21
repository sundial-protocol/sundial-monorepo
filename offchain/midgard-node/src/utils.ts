import * as chalk_ from "chalk";

export const chalk = new chalk_.Chalk();

export const showTime = (d: Date): string => {
  return d
    .toLocaleString("en-US", {
      month: "2-digit",
      day: "2-digit",
      year: "numeric",
      hour: "2-digit",
      minute: "2-digit",
      second: "2-digit",
      hour12: false,
    })
    .replace(/\//g, ".");
};

const logWithTime = (
  color: chalk_.ChalkInstance,
  label: string,
  msg: string
) => {
  const now = new Date();
  const timeStr = showTime(now);
  console.log(
    `${color(chalk.bold(`${timeStr}\u0009${label}`))}${
      label === "" ? "" : " "
    }${color(msg)}`
  );
};

export const logSuccess = (msg: string) => {
  logWithTime(chalk.green, "SUCCESS!", msg);
};

export const logWarning = (msg: string, quiet?: true) => {
  if (!quiet) {
    logWithTime(
      chalk.yellow,
      "WARNING",
      `
${msg}`
    );
  }
};

export const logAbort = (msg: string) => {
  logWithTime(
    chalk.red,
    "ABORT",
    `
${msg}`
  );
};

export const logDim = (msg: string) => {
  logWithTime(chalk.dim, "", msg);
};

export const logInfo = (msg: string) => {
  logWithTime(
    chalk.blue,
    "INFO",
    `
${msg}`
  );
};

export const isHexString = (str: string): boolean => {
  const hexRegex = /^[0-9A-Fa-f]+$/;
  return hexRegex.test(str);
};
