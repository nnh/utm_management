function fetchTextPublicIPs_(url: string): string[][] {
  try {
    const response: GoogleAppsScript.URL_Fetch.HTTPResponse =
      UrlFetchApp.fetch(url);
    const textData: string = response.getContentText();
    const textValues: string[][] = Utilities.parseCsv(textData);
    return textValues;
  } catch (error) {
    console.log('Failed to fetch IP ranges', error);
    return [];
  }
}
function outputTextIPRanges_(
  url: string,
  sheetName: string,
  userInfo: string
): void {
  const outputSheet: GoogleAppsScript.Spreadsheet.Sheet | null =
    getOutputSheet_(sheetName);
  if (outputSheet === null) {
    return;
  }
  const range: string[][] = fetchTextPublicIPs_(url);
  if (range.length === 0) {
    console.log('No IP ranges found');
    return;
  }
  const temp: string[][] = range.map(([ip]) => [ip, userInfo]);
  const outputValues: string[][] = [outputHeader, ...temp];
  outputSheetValues_(outputSheet, outputValues);
}
