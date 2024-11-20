import os
import json
import markdown
from weasyprint import HTML

def load_config(config_file):
    """設定ファイルを読み込む関数"""
    with open(config_file, 'r', encoding='utf-8') as f:
        config = json.load(f)
    return config

def convert_md_files_in_directory(input_directory, output_directory=None):
    """指定ディレクトリ内のすべてのMarkdownファイルをPDFに変換する関数"""
    css_file = "styles.css"
    # ホームディレクトリを取得
    home_directory = os.path.expanduser("~")
    
    # 入力ディレクトリの絶対パスを作成
    input_dir = os.path.join(home_directory, input_directory)
    
    # 出力ディレクトリが指定されていない場合は、入力ディレクトリと同じ場所に保存
    if output_directory:
        output_dir = os.path.join(home_directory, output_directory)
    else:
        output_dir = input_dir

    # 出力ディレクトリが存在しない場合は作成
    os.makedirs(output_dir, exist_ok=True)
    # 指定されたディレクトリ内の全ての.mdファイルを取得
    for filename in os.listdir(input_dir):
        if filename.endswith('.md'):
            input_file = os.path.join(input_dir, filename)
            
            # 出力ファイルのパスを設定
            if output_dir is None:
                output_dir = input_dir  # 出力先ディレクトリが指定されていなければ入力ディレクトリに保存
            output_file = os.path.join(output_dir, filename.replace('.md', '.pdf'))
            
            # すでにPDFファイルが存在するかをチェック
            if os.path.exists(output_file):
                print(f"既にPDFファイルが存在します: {output_file} - スキップします")
                continue  # PDFがすでに存在する場合はスキップ
            
            # 入力ファイルの読み込み
            with open(input_file, 'r', encoding='utf-8') as f:
                markdown_text = f.read()
            
            # MarkdownをHTMLに変換
            html_text = markdown.markdown(markdown_text)
            
            # HTMLをPDFに変換
#            HTML(string=html_text).write_pdf(output_file)
            HTML(string=html_text, base_url=os.path.dirname(css_file)).write_pdf(output_file, stylesheets=[css_file])
            print(f"PDFが作成されました: {output_file}")

# 設定ファイルからディレクトリを読み込む
config = load_config("config.json")

# 例の使い方：設定ファイルのディレクトリを使って変換
convert_md_files_in_directory(config["input_directory"], config["output_directory"])
