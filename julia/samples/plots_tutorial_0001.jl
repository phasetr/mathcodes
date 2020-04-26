# https://docs.juliaplots.org/latest/tutorial/
using Dates
using Plots; gr()
using PackageCompiler

datetime = Dates.format(now(), "yyyymmdd-HHMMSS")
println(datetime)
ws = "workspace/"
tgtdir = ws * datetime
mkpath(tgtdir)

x = 1:10; y = rand(10); # These are the plotting data
plot(x, y)
savefig(tgtdir * "/001.png")

x = 1:10; y = rand(10, 2) # 2 columns means two lines
plot(x, y)
savefig(tgtdir * "/002.png")

z = rand(10)
plot!(x, z) # 前の plot に追加する
savefig(tgtdir * "/003.png")

# 上のコードと同じコード: p = plot のところがポイント
x = 1:10; y = rand(10, 2) # 2 columns means two lines
p = plot(x, y)
z = rand(10)
plot!(p, x, z)
savefig(tgtdir * "/004.png")

# Plot Attributes
x = 1:10; y = rand(10, 2) # 2 columns means two lines
plot(x, y, title = "Two Lines",
     label = ["Line 1" "Line 2"],
     lw = 3)
savefig(tgtdir * "/005.png")

xlabel!("My x label") # 上のプロットを修正する
savefig(tgtdir * "/006.png")

gr() # Set the backend to GR
# This plots using GR
plot(x, y, title = "This is Plotted using GR")
savefig(tgtdir * "/007.png")
savefig(tgtdir * "/007_myplot.png") # Saves the CURRENT_PLOT as a .png
savefig(p, tgtdir * "/007_myplot.pdf") # Saves the plot from p as a .pdf vector graphic

gr() # We will continue onward using the GR backend
plot(x, y, seriestype = :scatter, title = "My Scatter Plot")
savefig(tgtdir * "/008.png")

scatter(x, y, title = "My Scatter Plot")
savefig(tgtdir * "/009.png")

# Plotting in Scripts
# プロットを出したいなら `display` を使うといいらしい
#display(plot(x, y))

# Combining Multiple Plots as Subplots
x = 1:10
y = rand(10, 4)
plot(x, y, layout = (4, 1))
savefig(tgtdir * "/010.png")

p1 = plot(x, y) # Make a line plot
p2 = scatter(x, y) # Make a scatter plot
p3 = plot(x, y, xlabel = "This one is labelled", lw = 3, title = "Subtitle")
p4 = histogram(x, y) # Four histograms each with 10 points? Why not!
plot(p1, p2, p3, p4, layout = (2, 2), legend = false)
savefig(tgtdir * "/011.png")

# https://docs.juliaplots.org/latest/tutorial/#Using-User-Recipes-1
# Pkg.add("StatsPlots")
using StatsPlots # Required for the DataFrame user recipe
# Now let's create the DataFrame
using DataFrames
df = DataFrame(a = 1:10, b = 10 * rand(10), c = 10 * rand(10))
# Plot the DataFrame by declaring the points by the column names
@df df plot(:a, [:b :c]) # x = :a, y = [:b :c]. Notice this is two columns!
savefig(tgtdir * "/012.png")

@df df scatter(:a, :b, title = "My DataFrame Scatter Plot!") # x = :a, y = :b
savefig(tgtdir * "/013.png")

# https://docs.juliaplots.org/latest/tutorial/#Using-a-Type-Recipe-1
using Distributions
plot(Normal(3, 5), lw = 3)
savefig(tgtdir * "/014.png")

# https://docs.juliaplots.org/latest/tutorial/#Using-Series-Recipes-1
y = rand(100, 4) # Four series of 100 points each
violin(["Series 1" "Series 2" "Series 3" "Series 4"], y, leg = false)
savefig(tgtdir * "/015.png")
boxplot!(["Series 1" "Series 2" "Series 3" "Series 4"], y, leg = false)
savefig(tgtdir * "/016.png")
