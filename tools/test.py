import markdown
from weasyprint import HTML

def convert_md_to_pdf(input_file, output_file=None):
    # 入力ファイルの読み込み
    with open(input_file, 'r', encoding='utf-8') as f:
        markdown_text = f.read()
    
    # MarkdownをHTMLに変換
    html_text = markdown.markdown(markdown_text)
    
    # PDFの出力ファイル名を設定（指定がなければ同じディレクトリに保存）
    if output_file is None:
        output_file = input_file.replace('.md', '.pdf')
    
    # HTMLをPDFに変換
    HTML(string=html_text).write_pdf(output_file)
    print(f"PDFが作成されました: {output_file}")

# 例の使い方
convert_md_to_pdf("aaa.md")
