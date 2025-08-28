succeeded=0
failed=0
for file in *.sml; do
  filename=${file%.*}
  if $SMLNETPATH/bin/smlnet.sh $filename; then
    if cmp --silent $filename.il $filename.il.expected; then
      echo "$filename succeeded"
      succeeded=$((succeeded+1))
    else
      echo "$filename failed"
      echo "Diff:"
      diff $filename.il $filename.il.expected
      failed=$((failed+1))
    fi
  else
    failed=$((failed+1))
  fi
done

echo Succeeded: $succeeded Failed: $failed