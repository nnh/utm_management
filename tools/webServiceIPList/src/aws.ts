interface IPPrefix {
  ip_prefix?: string; // IPv4 プレフィックス (オプショナル)
  ipv6_prefix?: string; // IPv6 プレフィックス (オプショナル)
  region: string;
  service: string;
  network_border_group: string;
}

// 全体データのインターフェース
interface AWSIPData {
  syncToken: string;
  createDate: string;
  prefixes: IPPrefix[];
}

function fetchAwsIPRanges_(url: string) {
  try {
    const jsonResponse = fetchJson_(url);
    const typedResponse = jsonResponse as AWSIPData;
    return typedResponse;
  } catch (error) {
    console.log('Failed to fetch IP ranges', error);
    return { prefixes: [], createDate: '', syncToken: '' };
  }
}
function outputAwsIPRanges_(): void {
  const outputSheet: GoogleAppsScript.Spreadsheet.Sheet | null =
    getOutputSheet_('ipRange_aws');
  if (outputSheet === null) {
    return;
  }
  const url: string = 'https://ip-ranges.amazonaws.com/ip-ranges.json';
  const range: AWSIPData = fetchAwsIPRanges_(url);
  if (range.prefixes.length === 0) {
    console.log('No IP ranges found');
    return;
  }
  const temp: readonly [string, string][] = range.prefixes
    .filter(value => value.ip_prefix) // ip_prefix が存在するものだけをフィルタ
    .map(
      value =>
        [value.ip_prefix!, `AWS, ${value.service}, ${value.region}`] as const
    ); // `as const` を使用してタプルを明示

  const outputBodyMap: Map<string, string> = new Map<string, string>(temp);
  const outputBody = Array.from(outputBodyMap);
  const outputValues: string[][] = [outputHeader, ...outputBody];
  outputSheetValues_(outputSheet, outputValues);
}
