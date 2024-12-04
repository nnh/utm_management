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
  const outputBodyMerge: string[][] = targetSheets
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
  const outputBodyExcludeIpv6: string[][] = outputBodyMerge
    .map(([ipWithMask, user]) =>
      isValidIPv4_(ipWithMask) ? [ipWithMask, user] : ['', '']
    )
    .filter(([ip, _]) => ip !== '');
  const outputBody: string[][] = outputBodyExcludeIpv6.map(
    ([ipWithMask, user]) => {
      const [startIp, endIp] = cidrToRange_(ipWithMask);
      const startIpOctets = splitIPAddress_(startIp);
      const endIpOctets = splitIPAddress_(endIp);
      return [
        ipWithMask,
        user,
        startIp,
        endIp,
        ...startIpOctets,
        ...endIpOctets,
      ];
    }
  );
  const sortedOutputBody = outputBody.sort((a, b) => {
    const [aOctet1, aOctet2, aOctet3, aOctet4] = a.slice(4, 8).map(Number);
    const [bOctet1, bOctet2, bOctet3, bOctet4] = b.slice(4, 8).map(Number);

    // Octet1から順番に比較
    if (aOctet1 !== bOctet1) return aOctet1 - bOctet1;
    if (aOctet2 !== bOctet2) return aOctet2 - bOctet2;
    if (aOctet3 !== bOctet3) return aOctet3 - bOctet3;
    return aOctet4 - bOctet4;
  });
  const outputHeader: string[] = [
    'ip',
    'User',
    'Start IP',
    'End IP',
    'Start_Octet1',
    'Start_Octet2',
    'Start_Octet3',
    'Start_Octet4',
    'End_Octet1',
    'End_Octet2',
    'End_Octet3',
    'End_Octet4',
  ];
  const outputValues: string[][] = [outputHeader, ...sortedOutputBody];
  outputSheetValues_(outputSheet, outputValues);

  /*  const outputBody: string[][] = outputBodyMerge.map(([ipWithMask, user]) => {
    const [ip, subnetMask] = ipWithMask.split('/');
    const [octet1, octet2, octet3, octet4] = ip.split('.');
    return [ipWithMask, user, octet1, octet2, octet3, octet4, subnetMask];
  });
  const sortedOutputBody = outputBody.sort((a, b) => {
    const [aOctet1, aOctet2, aOctet3, aOctet4] = a.slice(2, 6).map(Number);
    const [bOctet1, bOctet2, bOctet3, bOctet4] = b.slice(2, 6).map(Number);

    // Octet1から順番に比較
    if (aOctet1 !== bOctet1) return aOctet1 - bOctet1;
    if (aOctet2 !== bOctet2) return aOctet2 - bOctet2;
    if (aOctet3 !== bOctet3) return aOctet3 - bOctet3;
    return aOctet4 - bOctet4;
  });

  // ヘッダーとデータを結合
  const outputHeader: string[] = [
    'ip',
    'User',
    'Octet1',
    'Octet2',
    'Octet3',
    'Octet4',
    'SubnetMask',
  ];
  const outputValues: string[][] = [outputHeader, ...sortedOutputBody];
  outputSheetValues_(outputSheet, outputValues);*/
}
function cidrToRange_(cidr: string): string[] {
  const [ip, mask] = cidr.split('/');
  const subnetMask = parseInt(mask, 10);

  // IP を 32 ビット整数に変換
  const ipParts = ip.split('.').map(Number);
  const ipInt =
    (ipParts[0] << 24) | (ipParts[1] << 16) | (ipParts[2] << 8) | ipParts[3];

  // ネットワークアドレスとブロードキャストアドレスを計算
  const networkMask = ~(2 ** (32 - subnetMask) - 1);
  const networkAddress = ipInt & networkMask;
  const broadcastAddress = networkAddress | ~networkMask;

  // 32 ビット整数を IP アドレスに変換
  const intToIP = (int: number): string =>
    [
      (int >>> 24) & 0xff,
      (int >>> 16) & 0xff,
      (int >>> 8) & 0xff,
      int & 0xff,
    ].join('.');

  return [intToIP(networkAddress), intToIP(broadcastAddress)];
}
function splitIPAddress_(ip: string): string[] {
  const octets = ip.split('.');
  if (octets.length !== 4) {
    throw new Error('Invalid IP address format');
  }
  return octets;
}
