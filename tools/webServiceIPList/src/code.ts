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
    console.log(`${sheetName}  not found`);
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
function outputAwsIPRanges() {
  outputAwsIPRanges_();
}
function outputMicrosoftIPRanges() {
  outputMicrosoftIPRanges_();
}
function outputZoomIPRanges() {
  const url: string = 'https://assets.zoom.us/docs/ipranges/Zoom.txt';
  const sheetName: string = 'ipRange_zoom';
  const userInfo: string = 'Zoom Communications, Inc.';
  outputTextIPRanges_(url, sheetName, userInfo);
}
function outputCloudflareIPRanges() {
  const url: string = 'https://www.cloudflare.com/ips-v4/#';
  const sheetName: string = 'ipRange_cloudflare';
  const userInfo: string = 'Cloudflare, Inc.';
  outputTextIPRanges_(url, sheetName, userInfo);
}
function mergeIpRanges() {
  mergeIpRanges_();
}
