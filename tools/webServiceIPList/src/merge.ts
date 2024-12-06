function isValidIPv4_(ip: string): boolean {
  const ipv4CidrRegex =
    /^(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)(\/([0-9]|[12][0-9]|3[0-2]))?$/;
  return ipv4CidrRegex.test(ip);
}
function mergeIpRanges_(): void {
  const outputSheet: GoogleAppsScript.Spreadsheet.Sheet | null =
    getOutputSheet_('whois');
  if (outputSheet === null) {
    console.log('No output sheet found');
    return;
  }
  const sheetList: GoogleAppsScript.Spreadsheet.Sheet[] =
    SpreadsheetApp.getActiveSpreadsheet().getSheets();
  const targetSheets: GoogleAppsScript.Spreadsheet.Sheet[] = sheetList.filter(
    sheet => sheet.getName().startsWith('ipRange_')
  );
  if (targetSheets.length === 0) {
    console.log('No target sheets found');
    return;
  }
  const outputBody: string[][] = targetSheets
    .map(sheet => {
      const lastRow: number = sheet.getLastRow();
      const lastCol: number = sheet.getLastColumn();
      if (lastRow < 2) {
        console.log(`No data found: ${sheet.getName()}`);
        return [];
      }
      const values: string[][] = sheet
        .getRange(2, 1, lastRow - 1, lastCol)
        .getValues();
      return values;
    })
    .flat();
  /*const outputBody: string[][] = outputBodyMerge
    .map(([ipWithMask, user]) =>
      isValidIPv4_(ipWithMask) ? [ipWithMask, user] : ['', '']
    )
    .filter(([ip, _]) => ip !== '');*/
  const outputHeader: string[] = ['ip', 'User'];
  outputSheetValues_(outputSheet, [outputHeader, ...outputBody]);
}
