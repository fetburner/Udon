structure StringMap = BinaryMapFn (struct
  open String
  type ord_key = string
end)
