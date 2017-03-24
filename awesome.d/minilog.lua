return {
    logger =
        function (level)
            local levels = {off=0, info=30, fine=50}
            local l = {level=level}
            l.mt = {}
            l.mt.__index = function(table, key)
                if not levels[key] or not levels[table.level] then
                    error("Logging level not found")
                end
                return levels[key] <= levels[table.level]
            end
            l.print = function(level, ...)
                if l[level] then
                    print(...)
                end
            end
            return setmetatable(l, l.mt)
        end
}

