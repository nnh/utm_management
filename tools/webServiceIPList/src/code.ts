const outputHeader: string[] = ['ip', 'User'];
function outputSheetValues_(
  sheet: GoogleAppsScript.Spreadsheet.Sheet,
  values: string[][]
) {
  sheet.clearContents();
  sheet.getRange(1, 1, values.length, values[0].length).setValues(values);
}
function getOutputSheet_(
  sheetName: string
): GoogleAppsScript.Spreadsheet.Sheet | null {
  const outputSheet: GoogleAppsScript.Spreadsheet.Sheet | null =
    SpreadsheetApp.getActiveSpreadsheet().getSheetByName(sheetName);
  if (outputSheet === null) {
    console.error(`${sheetName}  not found`);
    return null;
  }
  return outputSheet;
}
function fetchJson_(url: string): any {
  const response: GoogleAppsScript.URL_Fetch.HTTPResponse =
    UrlFetchApp.fetch(url); // synchronous request
  const jsonResponse: any = JSON.parse(response.getContentText()); // Parse the JSON response
  return jsonResponse;
}
function outputGoogIPRanges() {
  outputGoogIPRanges_();
}
function outputGoogleCloudIPRanges() {
  outputGoogleCloudIPRanges_();
}
