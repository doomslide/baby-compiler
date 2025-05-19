define i64 @dce_after_fold(i64 %param0)
{
%temp0 = add i64 15, %param0
ret i64 %temp0
} 