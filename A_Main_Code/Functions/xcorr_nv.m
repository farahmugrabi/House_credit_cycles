function [r,lags] = xcorr_nv(x,y,k)
    try
        % Sintaxis nombre–valor clásica
        [r,lags] = crosscorr(x,y,'NumLags',k);
    catch
        try
            % Sintaxis posicional (versiones viejas)
            [r,lags] = crosscorr(x,y,k);
        catch
            % Sin Econometrics Toolbox: usa xcorr (Signal Processing)
            [r,lags] = xcorr(x,y,k,'coeff');
        end
    end
end