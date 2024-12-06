interface GoogIpPrefix {
  ipv4Prefix?: string; // IPv4 アドレスのプレフィックス
  ipv6Prefix?: string; // IPv6 アドレスのプレフィックス
}

interface GoogIpRangesResponse {
  syncToken: string; // 同期トークン
  creationTime: string; // データ生成時刻 (ISO 8601)
  prefixes: GoogIpPrefix[]; // IP プレフィックスのリスト
}
interface GoogleCloudIpRanges {
  ipv4Prefix: string; // The IP range in CIDR format (e.g., "203.0.113.0/24")
  service: string; // The name of the Google service using the IP range (e.g., "Google Cloud Storage")
  scope: string; // The type of scope (e.g., "REGION" or "GLOBAL")
}

interface GoogleCloudIpRangesResponse {
  creationTime: string; // Timestamp when the IP range data was created
  services: string[]; // List of Google services covered by the IP ranges
  prefixes: GoogleCloudIpRanges[]; // Array of IP ranges
}

function fetchGoogleCloudIPRanges_(url: string) {
  try {
    const jsonResponse = fetchJson_(url);
    const typedResponse = jsonResponse as GoogleCloudIpRangesResponse;
    return typedResponse;
  } catch (error) {
    console.log('Failed to fetch IP ranges', error);
    return { prefixes: [], services: [], creationTime: '' };
  }
}

function outputGoogleCloudIPRanges_(): void {
  const outputSheet: GoogleAppsScript.Spreadsheet.Sheet | null =
    getOutputSheet_('ipRange_googleCloud');
  if (outputSheet === null) {
    return;
  }
  const url: string = 'https://www.gstatic.com/ipranges/cloud.json';
  const range: GoogleCloudIpRangesResponse = fetchGoogleCloudIPRanges_(url);
  if (range.prefixes.length === 0) {
    console.log('No IP ranges found');
    return;
  }
  const temp: string[][] = range.prefixes
    .map(value => [
      value.hasOwnProperty('ipv4Prefix') ? value.ipv4Prefix : '',
      `${value.service}, ${value.scope}`,
    ])
    .filter(([ip, _]) => ip !== '');
  const outputValues: string[][] = [outputHeader, ...temp];
  outputSheetValues_(outputSheet, outputValues);
}
function fetchGoogleIPRanges_(url: string) {
  try {
    const jsonResponse = fetchJson_(url);
    const typedResponse = jsonResponse as GoogIpRangesResponse;
    return typedResponse;
  } catch (error) {
    console.log('Failed to fetch IP ranges', error);
    return { prefixes: [], syncToken: '', creationTime: '' };
  }
}

function outputGoogIPRanges_(): void {
  const outputSheet: GoogleAppsScript.Spreadsheet.Sheet | null =
    getOutputSheet_('ipRange_google');
  if (outputSheet === null) {
    return;
  }
  const url: string = 'https://www.gstatic.com/ipranges/goog.json';
  const range: GoogIpRangesResponse = fetchGoogleIPRanges_(url);
  if (range.prefixes.length === 0) {
    console.log('No IP ranges found');
    return;
  }
  const temp: string[][] = range.prefixes
    .map(value => [
      value.hasOwnProperty('ipv4Prefix') && value.ipv4Prefix
        ? value.ipv4Prefix
        : '',
      'Google LLC',
    ])
    .filter(([ip, _]) => ip !== '');
  const outputValues: string[][] = [outputHeader, ...temp];
  outputSheetValues_(outputSheet, outputValues);
}
