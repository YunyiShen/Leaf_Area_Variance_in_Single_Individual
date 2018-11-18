T0 = 0.1; % tollerence of temp
Topt = 30; % opt temp for photosys
TL0 = 20; % average leaves Temp
Imax = 40; % max iridiation 
Isat = 15; % half sunlight saturation
Pmax = 30; % max Photosynthesis rate
Ivar = 0.1;
meanS = 1;
nrep = 100000;
%Svar = 5;
%P = @(I,TL) I.*(gaussmf(TL,[T0 Topt])); % photosys as function of temp
P = @(I,TL) Pmax * (I./(I + Isat)) .*(gaussmf(TL,[T0 Topt])); % photosys as function of temp M-M to sunlight
TL = @(I,S) I.*(S) + TL0; % temp as function of shape factor and Iridiation
timestep = 0:0.01:pi;
It = repmat(Imax * sin(timestep),nrep,1) + Ivar * randn(nrep,length(timestep));
It = It'.* (It'>=0);
Svar = 0.1:0.1:2;
variation = zeros(0,100);
means = zeros(0,100);
for i = 1:length(Svar)
    Ss = (Svar(i)) * randn(1,nrep) + meanS;
    Ss = repmat(Ss,length(timestep),1);
    TotalP = sum(P(It,TL(It,Ss)));
    variation(i) = sqrt(var(TotalP));
    means(i) = mean(TotalP);
end
figure
plot(Svar,means);
figure
plot(Svar,variation./means);
