#!/bin/bash

declare -a fonts_name
declare -a fonts_url

fonts_name+=("Inconsolata")
fonts_url+=('http://levien.com/type/myfonts/Inconsolata.otf')

fonts_name+=("Noto_Sans")
fonts_url+=('https://noto-website-2.storage.googleapis.com/pkgs/NotoSans-hinted.zip')

fonts_name+=("Noto_Serif")
fonts_url+=('https://noto-website-2.storage.googleapis.com/pkgs/NotoSerif-hinted.zip')

fonts_name+=("Noto_Sans_Display")
fonts_url+=('https://noto-website-2.storage.googleapis.com/pkgs/NotoSansDisplay-hinted.zip')

fonts_name+=("Noto_Serif_Display")
fonts_url+=('https://noto-website-2.storage.googleapis.com/pkgs/NotoSerifDisplay-hinted.zip')

fonts_name+=("Noto_Mono")
fonts_url+=('https://noto-website-2.storage.googleapis.com/pkgs/NotoMono-hinted.zip')

fonts_name+=("Noto_Sans_Mono")
fonts_url+=('https://noto-website-2.storage.googleapis.com/pkgs/NotoSansMono-hinted.zip')

fonts_name+=("Noto_Sans_CJK_JP")
fonts_url+=('https://noto-website-2.storage.googleapis.com/pkgs/NotoSansCJKjp-hinted.zip')

fonts_name+=("Noto_Serif_CJK_JP")
fonts_url+=('https://noto-website-2.storage.googleapis.com/pkgs/NotoSerifCJKjp-hinted.zip')

fonts_name+=("IPAexFont")
fonts_url+=('https://oscdl.ipa.go.jp/IPAexfont/IPAexfont00301.zip')

fonts_name+=("IPAFont")
fonts_url+=('https://ipafont.ipa.go.jp/old/ipafont/IPAfont00303.php')

#fonts_name+=("MPlus")
#fonts_url+=('https://ja.osdn.net/dl/mplus-fonts/mplus-TESTFLIGHT-063.tar.xz')

fonts_name+=("Migu1P")
fonts_url+=('https://ja.osdn.net/dl/mix-mplus-ipa/migu-1p-20150712.zip')

fonts_name+=("Migu1C")
fonts_url+=('https://ja.osdn.net/dl/mix-mplus-ipa/migu-1c-20150712.zip')

fonts_name+=("Migu1M")
fonts_url+=('https://ja.osdn.net/dl/mix-mplus-ipa/migu-1m-20150712.zip')

fonts_name+=("MugenPlus")
fonts_url+=('https://osdn.jp/downloads/users/8/8597/mgenplus-20150602.7z')

fonts_name+=("Sawarabi-gothic")
fonts_url+=('https://ja.osdn.net/dl/sawarabi-fonts/sawarabi-gothic-otf-20161015.zip')

fonts_name+=("Sawarabi-mincho")
fonts_url+=('https://ja.osdn.net/dl/sawarabi-fonts/sawarabi-mincho-otf-20171015.zip')

fonts_name+=("Honoka-mincho")
fonts_url+=('http://font.gloomy.jp/dl-font-s5a4ik5w/honoka-min.zip')

fonts_name+=("Honoka-gothic")
fonts_url+=('http://font.gloomy.jp/dl-font-s5a4ik5w/honoka-marugo.zip')

fonts_name+=("HonokaAntique-maru")
fonts_url+=('http://font.gloomy.jp/dl-font-s5a4ik5w/antique-maru.zip')

fonts_name+=("HonokaAantique-kaku")
fonts_url+=('http://font.gloomy.jp/dl-font-s5a4ik5w/antique-kaku.zip')

fonts_name+=("ShinComic")
fonts_url+=('http://www.font910.jp/freefont_i6wiwk5/f910-shin-comic-2.04.zip')

fonts_name+=("Hannari-mincho")
fonts_url+=('http://typingart.net/fontdata/hannari.zip --referer http://typingart.net/?p=44')

fonts_name+=("Kokoro-mincho")
fonts_url+=('http://typingart.net/fontdata/kokoro.zip --referer http://typingart.net/?p=46')

fonts_name+=("AozoraMincho")
fonts_url+=('http://blueskis.wktk.so/AozoraMincho/archive/v0.1/aozoramincho-readme-ttf.zip')

fonts_name+=("Source-Han-Code-JP")
fonts_url+=('https://github.com/adobe-fonts/source-han-code-jp/archive/2.011R.zip')

fonts_name+=("Kazesawa")
fonts_url+=('https://github.com/kazesawa/kazesawa/releases/download/alpha-v1/kazesawa.zip')

fonts_name+=("Noto_Emoji")
fonts_url+=('https://noto-website-2.storage.googleapis.com/pkgs/NotoEmoji-unhinted.zip')

fonts_name+=("PowerlineSymbols")
fonts_url+=('https://github.com/powerline/powerline/raw/develop/font/PowerlineSymbols.otf')

fonts_name+=("DejaVu")
fonts_url+=('http://sourceforge.net/projects/dejavu/files/dejavu/2.37/dejavu-fonts-ttf-2.37.tar.bz2')

#--------------

font_dir="$(pwd)"
temp_dir="$(mktemp --directory -t fonttemp.XXXXX)"

#--------------

echo "Font Directory: ${font_dir}"
echo "Working Directory: ${temp_dir}"

cd "${temp_dir}"

for idx in "${!fonts_name[@]}"; do
echo "Downloading from ${fonts_url[${idx}]} ..."
curl -O -L ${fonts_url[${idx}]}
find . -type f \( -name *.zip -or -name *.7z \) -exec 7z e {} -o"${font_dir}/${fonts_name[${idx}]}" \; -exec rm {} \;
find . -type f -name *.otf -exec mv {} "${font_dir}/${fonts_name[${idx}]}.otf" \;
done

rm -r "${temp_dir}"
