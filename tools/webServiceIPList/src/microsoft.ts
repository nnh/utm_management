function fetchMicrosoftPublicIPs(url: string): string[][] {
  try {
    const response: GoogleAppsScript.URL_Fetch.HTTPResponse =
      UrlFetchApp.fetch(url);
    const csvData: string = response.getContentText();
    const csvValues: string[][] = Utilities.parseCsv(csvData);
    return csvValues;
  } catch (error) {
    console.log('Failed to fetch IP ranges', error);
    return [];
  }
}
function outputMicrosoftIPRanges_(): void {
  const url: string =
    'https://download.microsoft.com/download/B/2/A/B2AB28E1-DAE1-44E8-A867-4987FE089EBE/msft-public-ips.csv';
  const outputSheet: GoogleAppsScript.Spreadsheet.Sheet | null =
    getOutputSheet_('ipRange_microsoft');
  if (outputSheet === null) {
    return;
  }
  const range: string[][] = fetchMicrosoftPublicIPs(url);
  if (range.length === 0) {
    console.log('No IP ranges found');
    return;
  }
  const temp: string[][] = range
    .map(([ip, _]) => [ip, `Microsoft Corporation`])
    .filter(([ip, _]) => ip !== '' && ip !== 'Prefix');
  const outputValues: string[][] = [outputHeader, ...temp];
  outputSheetValues_(outputSheet, outputValues);
}
