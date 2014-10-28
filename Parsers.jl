
module Parsers

 export getTocken, Grammar, Gmin, geof, cast, lift, -, |, +, mbind, mreturn, mfail, gfilter, starmax
 export ArithExpr, toDig, gdigit, gnumber, snumber, amul, asum
 export GArith, GExpr

 empty = []

 getTocken(s) = if length(s) == 0
                 empty
                else
                 [(s[1],s[2:end])]
                end
 getTocken(g, s) = getTocken(s)

 abstract Grammar

 type Gmin <: Grammar
 end

 geof(g::Grammar, s) = if length(s) == 0
                [((),s)]
              else
                empty
              end
 

 lift(g::Grammar, f, p) = (g,s) -> map((r) -> (f(r[1]),r[2]), p(g,s))

 >(g::Grammar, p1, p2) = (g,s) ->
          reduce((a,r) -> let
              local v
              local s
              v,s = r
              [a, p2(g,s)]
            end, empty, p1(g,s))
 <(g::Grammar, p1, p2) = (g,s) ->
          reduce((a,r) -> let
              local v
              local s
              v,s = r
              [a, map((r) -> (v,r[2]), p2(g,s))]
            end, empty, p1(g,s))

 -(g::Grammar, p) = lift(g, (x) -> [x], p)
 -(g::Grammar, p1, p2) = (g,s) ->
          reduce((a,r) -> let
              local v
              local s
              v,s = r
              [a, map((r) -> ([v,r[1]],r[2]), p2(g,s))]
            end, empty, p1(g,s))
 -(g::Grammar, p1, p2, px...) =  (-(g, p1, -(g, p2, px...)))

 lift(g::Grammar, f, p, px...) = lift(g, (t) -> apply(f,t), -(g,p,px...))
 
 |(g::Grammar, p) = p
 |(g::Grammar, p1, p2) = (g,s) -> [p1(g,s),p2(g,s)]
 |(g::Grammar, p1, p2, px...) =  |(g::Grammar, |(g::Grammar, p1, p2), px...)

 +(g::Grammar, p) = p
 +(g::Grammar, p1, p2) = (g,s) -> let r = p1(g,s)
                                   if length(r) == 0
                                     p2(g,s)
                                   else
                                     r
                                   end
                                  end
 +(g::Grammar, p1, p2, px...) =  +(g::Grammar, +(g::Grammar, p1, p2), px...)

 mreturn(g::Grammar, v) = (g,s) -> [(v,s)] 
 mbind(g::Grammar, p, fp) = (g,s) -> reduce((a,v)->[a,fp(g,v[1])(g,v[2])], empty, p(g,s))
 mfail(g::Grammar, s) = (_g,_s) -> empty

 gfilter(g::Grammar, f, p) = (g,s) -> filter((r) -> f(r[1]),  p(g,s))

 cast(g::Grammar,p) = (_,s) -> p(g,s)

 starmax(g::Grammar,p,f=(x) -> [x[1],x[2]],i=[]) = (g,s) -> (+(g, (lift(g,f,-(g, p, starmax(g,p,f,i)))), mreturn(g,i)))(g,s)




 abstract Arith <: Grammar
 type ArithExpr <: Arith
 end

 toDig(base)= (g,n) -> (g, s) -> let r = if('0' <= n <= '9')
                                         n - '0'
                               elseif('a'<=n<='z')
                                         n - ('a'-10)
                               elseif('A'<=n<='Z')
                                         n - ('A'-10)
                               else
                                         base
                               end
                      if(r < base) ;
                          [(r,s)]
                       else
                          empty
                      end 
                     end
 gdigit(g::Arith, base) = mbind(g,getTocken,toDig(base))

 gnumber(g::Arith, base) = lift(g, (x) -> x[1], starmax(g, gdigit(g,base),(a) -> (a[2][1]+a[1]*a[2][2],a[2][2]*base),(0,1)))

 snumber(g::Arith, s) = gnumber(g, 10)(g,s)

 amul(g::Arith,s) = lift(g, (x) -> x[1]*x[2], (-(g, snumber, +(g, >(g, gfilter(g, (c) -> c == '*', getTocken), amul), mreturn(g,1)))))(g,s)
 asum(g::Arith,s) = lift(g, (x) -> x[1]+x[2], (-(g, amul, +(g, >(g, gfilter(g, (c) -> c == '+', getTocken), asum), mreturn(g,0)))))(g,s)
 

 abstract GArith <: Arith

 type GExpr <: GArith
 end

 snumber(g::GArith, s) = mbind(g,
                   cast(ArithExpr(),snumber),
                   (g,v) -> (+(g, (>(g, gfilter(g, (c) -> c == '#', getTocken), gnumber(g,v))),
                                   mreturn(g,v))))(g,s)
end

importall Parsers
