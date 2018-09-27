function m = good_max(xs)
    if isempty(xs)
        return;
    elseif length(xs) == 1
        m = xs(1);
    else
        tl_ans = good_max(xs(2:end));
        if xs(1) > tl_ans
            m = xs(1);
        else
            m = tl_ans;
        end
    end
end