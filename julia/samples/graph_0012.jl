using Plots; gr();
x = 1.0:0.1:10.0
f(x) = sin(1) .- sin.(x) ./ x
y = f(x)
plot(x, y)
savefig("1.tmp.png")
