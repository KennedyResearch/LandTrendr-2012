function timestamp
  time = string(bin_date(systime()), format='(I4,I02,I02,"_",I02,I02,I02)')
  return, time 
end