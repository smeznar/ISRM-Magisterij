function Ie = Canny(picture)
    trash_low = 0.06;
    trash_high = 0.18;
    [mag, angle]= DeriveImage(picture);
    Imax = NonmaximaSurpression(mag, angle);
    [It,n] = bwlabel(Imax>trash_low);
    [y,x] = size(picture);
    Ie = zeros(y,x);
    for i = 1:n
      A = It==i;
      if max(max(mag(A))) >= trash_high
        Ie = Ie + A;
      end
    end
end

