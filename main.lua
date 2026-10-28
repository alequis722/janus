_type={
 "i8",
 "i16",
 "u8",
 "u16",
 "f16",
}

function isin(a,b)
 for i,s in pairs(b) do
  if dump(a)==dump(s) then
   return true
  end
 end
 return false
end

function check(err)
 if err~=nil then
  print(err)
  os.exit(1)
 end
 return
end

function dump(obj)
 if type(obj)=="table" then
  local res="{ "
  for i,s in pairs(obj) do
   res=res..dump(s)
   if i<#obj then
    res=res..', '
   end
  end
  res=res..' }'
  return res
 elseif type(obj)=="string" then
  local res='"'..obj..'"'
  return res
 end
 return tostring(obj)
end

function lex(code)
 local i=1
 local res={}
 local buf=""
 local keyword={
  "const",
  "var",
  "return",
 }
 while i<=#code do
  if code:sub(i,i)=='"' then
   i=i+1
   local c=1
   while i<#code do
    if code:sub(i,i)=='"' then c=c-1
    elseif code:sub(i,i)=='\n' then
     return nil,"String exceeds one line"
    end
    if c==0 then break end
    buf=buf..code:sub(i,i)
    i=i+1
   end
   if c~=0 then
    return nil,"Unfinished string"
   end
   res[#res+1]={"string",buf}
   buf=""
  elseif code:sub(i,i):match("%a") then
   while i<#code and (code:sub(i,i):match("[%a%d]") or code:sub(i,i)=='_') do
    buf=buf..code:sub(i,i)
    i=i+1
   end
   if isin(buf,keyword) or isin(buf,_type) then
    res[#res+1]=buf
   else
    res[#res+1]={"name",buf}
   end
   buf=""
  elseif code:sub(i,i):match("%d") then
   while i<#code and code:sub(i,i):match("%d") do
    buf=buf..code:sub(i,i)
    i=i+1
   end
   res[#res+1]={"number",tonumber(buf)}
   buf=""
  end
  if i<=#code then
   if code:sub(i,i)=='\n' then
    res[#res+1]="eol"
   elseif code:sub(i,i)=='(' then
    res[#res+1]="s_expr"
   elseif code:sub(i,i)==')' then
    res[#res+1]="e_expr"
   elseif code:sub(i,i)==',' then
    res[#res+1]="comma"
   elseif code:sub(i,i)=='+' then
    if code:sub(i+1,i+1):match("%d") and type(res[#res])~="table" then
     buf=''
     i=i+1
     while i<#code and code:sub(i,i):match("%d") do
      buf=buf..code:sub(i,i)
      i=i+1
     end
     res[#res+1]={"number",tonumber(buf)}
     buf=""
     i=i-1
    else
     res[#res+1]="add"
    end
   elseif code:sub(i,i)=='-' then
    if code:sub(i+1,i+1):match("%d") and type(res[#res])~="table" then
     buf='-'
     i=i+1
     while i<#code and code:sub(i,i):match("%d") do
      buf=buf..code:sub(i,i)
      i=i+1
     end
     res[#res+1]={"number",tonumber(buf)}
     buf=""
     i=i-1
    else
     res[#res+1]="sub"
    end
   elseif code:sub(i,i)=='*' then
    res[#res+1]="mul"
   elseif code:sub(i,i)=='/' then
    res[#res+1]="div"
   elseif code:sub(i,i)=='%' then
    res[#res+1]="mod"
   elseif code:sub(i,i)==':' then
    if i+1<=#code and code:sub(i+1,i+1)=='=' then
     i=i+1
     res[#res+1]="assign"
    else
     res[#res+1]="colon"
    end
   elseif code:sub(i,i)=='=' then
    res[#res+1]="equal"
   elseif code:sub(i,i)=='<' then
    if i+1<=#code and code:sub(i+1,i+1)=='=' then
     i=i+1
     res[#res+1]="lessequal"
    else
     res[#res+1]="less"
    end
   elseif code:sub(i,i)=='>' then
    if i+1<=#code and code:sub(i+1,i+1)=='=' then
     i=i+1
     res[#res+1]="moreequal"
    else
     res[#res+1]="more"
    end
   end
  end
  i=i+1
 end
 if res[#res]~="eol" then
  res[#res+1]="eol"
 end
 return res,nil
end

function parse(tokens)
 local res=tokens
 local err=nil
 res,err=parseParen(res)
 check(err)
 res,err=parseOp1(res)
 check(err)
 res,err=parseOp2(res)
 check(err)
 res,err=parseCall(res)
 check(err)
 res,err=parseAssign(res)
 check(err)
 res,err=parseKeyword(res)
 check(err)
 return res
end

function parseParen(tokens)
 local i=1
 local res={}
 while i<=#tokens do
  if tokens[i]=="s_expr" then
   i=i+1
   local a={}
   local b={}
   local err=nil
   local c=1
   while i<=#tokens do
    if tokens[i]=="s_expr" then
     c=c+1
    elseif tokens[i]=="e_expr" then
     c=c-1
    end
    if c==0 then break end
    a[#a+1]=tokens[i]
    i=i+1
    if i<=#tokens and tokens[i]=="comma" then
     b[#b+1],err=parse(a)
     check(err)
     a={}
     i=i+1
    end
   end
   if c~=0 then
    return nil,"paren: Unfinished expression"
   end
   b[#b+1],err=parse(a)
   check(err)
   res[#res+1]={"expr",b}
  elseif tokens[i]=="e_expr" then
   return nil,"Unexpected ')'"
  else
   res[#res+1]=tokens[i]
  end
  i=i+1
 end
 return res,nil
end

function parseOp1(tokens)
 local i=1
 local res={}
 while i<=#tokens do
  if tokens[i]=="mul" or tokens[i]=="div" or tokens[i]=="mod" then
   if type(res[#res])~="table" then
    return nil,"Unexpected token"
   end
   local lhs=res[#res]
   if i+1>#tokens then
    return nil,"Unfinished expression (*/%)"
   end
   if type(tokens[i+1])~="table" then
    return nil,"Unexpected token"
   end
   local rhs=tokens[i+1]
   res[#res]={tokens[i],lhs,rhs}
   i=i+1
  else
   res[#res+1]=tokens[i]
  end
  i=i+1
 end
 return res,nil
end

function parseOp2(tokens)
 local i=1
 local res={}
 while i<=#tokens do
  if tokens[i]=="add" or tokens[i]=="sub" then
   if type(res[#res])~="table" then
    return nil,"Unexpected token"
   end
   local lhs=res[#res]
   if i+1>#tokens then
    return nil,"Unfinished expression (+-)"
   end
   if type(tokens[i+1])~="table" then
    return nil,"Unexpected token"
   end
   local rhs=tokens[i+1]
   res[#res]={tokens[i],lhs,rhs}
   i=i+1
  else
   res[#res+1]=tokens[i]
  end
  i=i+1
 end
 return res,nil
end

function parseCall(tokens)
 local i=1
 local res={}
 while i<=#tokens do
  if type(tokens)=="table" and tokens[i][1]=="name" then
   if i+1<=#tokens and type(tokens[i+1])=="table" and tokens[i+1][1]=="expr" then
    i=i+1
    res[#res+1]={tokens[i-1][2],tokens[i][2]}
   else
    res[#res+1]=tokens[i]
   end
  else
   res[#res+1]=tokens[i]
  end
  i=i+1
 end
 return res,nil
end

function parseAssign(tokens)
 local i=1
 local res={}
 while i<=#tokens do
  if tokens[i]=="assign" then
   if type(res[#res])~="table" or res[#res][1]~="name" then
    return nil,"Expected a name"
   elseif i+1>#tokens or type(tokens[i+1])~="table" then
    return nil,"Expected an expression"
   end
   local name=res[#res][2]
   i=i+1
   res[#res]={"assign10",name,tokens[i]}
  elseif tokens[i]=="colon" then
   if type(res[#res])~="table" or res[#res][1]~="name" then
    return nil,"Expected a name"
   elseif i+1>#tokens or type(tokens[i+1])~="string" or not isin(tokens[i+1],_type) then
    return nil,"Expected a type after ':'"
   end
   local name=res[#res][2]
   local t=tokens[i+1]
   i=i+2
   if i>#tokens or type(tokens[i])~="string" or tokens[i]~="equal" then
    res[#res]={"assign01",name,t}
   else
    i=i+1
    res[#res]={"assign11",name,tokens[i],t}
   end
  else
   res[#res+1]=tokens[i]
  end
  i=i+1
 end
 return res,nil
end

function parseKeyword(tokens)
 local i=1
 local res={}
 while i<=#tokens do
  if tokens[i]=="return" then
   i=i+1
   if i>#tokens or type(tokens[i])~="table" then
    return nil,"Expected an expression"
   end
   res[#res+1]={"return",tokens[i]}
  elseif isin(tokens[i],{"const","var"}) then
   local cv=tokens[i]
   i=i+1
   if i>#tokens or type(tokens[i])~="table" or (tokens[i][1]:sub(1,6)~="assign" and tokens[i][1]~="name") then
    return nil,"Unfinished constant/variable declaration"
   end
   if (tokens[i][1]=="name" or tokens[i][1]=="assign01") and cv=="const" then
    return nil,"Constant '"..tokens[i][3].."' must be initialized"
   end
   if tokens[i][1]=="name" then
    res[#res+1]={cv.."00",tokens[i][2]}
   elseif tokens[i][1]=="assign10" or tokens[i][1]=="assign01" then
    res[#res+1]={cv..tokens[i][1]:sub(7,8),tokens[i][2],tokens[i][3]}
   elseif tokens[i][1]=="assign11" then
    res[#res+1]={cv.."11",tokens[i][2],tokens[i][3],tokens[i][4]}
   end
  else
   res[#res+1]=tokens[i]
  end
  i=i+1
 end
 return res,nil
end

local fin=io.open("main.jan")
local code=fin:read("*a")
fin:close()
local tokens,err=lex(code)
check(err)
print("tokens",dump(tokens))
local ast=parse(tokens)
print("ast",dump(ast))
return 0
