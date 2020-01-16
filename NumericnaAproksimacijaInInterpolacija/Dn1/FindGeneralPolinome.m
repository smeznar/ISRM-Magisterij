function [p] = FindGeneralPolinome(f,bf,tocke)
    mat = ones(length(tocke));
    for i = 1:length(tocke)
       if i<length(tocke)
           g = bf{i};
           mat(:,i) = -g(tocke)';
       else
           g = ones(length(tocke),1);
           g(1:2:length(tocke)) = -1;
           mat(:,i) = -g;
       end
    end
    px = -f(tocke)';
    p = mat\px;
end

