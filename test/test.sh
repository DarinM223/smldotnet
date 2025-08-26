succeeded=0
failed=0
for file in *.sml; do
  filename=${file%.*}
  $SMLNETPATH/bin/smlnet.sh $filename
  if cmp --silent $filename.il $filename.il.expected; then
    echo "$filename succeeded"
    succeeded=$((succeeded+1))
  else
    echo "$filename failed"
    echo "Diff:"
    diff $filename.il $filename.il.expected
    failed=$((failed+1))
  fi
done

echo Succeeded: $succeeded Failed: $failed