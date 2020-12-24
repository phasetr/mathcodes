# http://docs.juliaplots.org/latest/layouts/
using Plots; gr()

#%%
plot(
    [x -> sin(x - a) for a in range(0, π / 2, length = 5)], 0, 2π;
    palette = :Dark2_5,
)
#%%

#%%
function f(x, y)
    r = sqrt(x^2 + y^2)
    return cos(r) / (1 + r)
end
x = range(0, 2π, length = 30)
heatmap(x, x, f, c = :thermal)
#%%

#%%
palette(:tab10)
#%%

#%%
palette([:purple, :green], 7)
#%%
