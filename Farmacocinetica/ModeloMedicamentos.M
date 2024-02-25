%-------------------------------------------------------------------------------------------
%                         UNA DOSIS
%-------------------------------------------------------------------------------------------


% Parámetros del modelo
C0 = 100; % Concentración inicial del medicamento (mg/L) 
            % después de la dosis
k = 0.1; % Constante de eliminación (1/horas)

% Tiempo
tFinal = 24; % Duración de la simulación en horas
dt = 0.1; % Paso de tiempo en horas
t = 0:dt:tFinal; % Vector de tiempo

% Solución de la ecuación diferencial
C = C0 * exp(-k * t);

% Gráfico
figure;
plot(t, C, 'LineWidth', 2);
xlabel('Tiempo (horas)');
ylabel('Concentración del medicamento (mg/L)');
title('Dinámica de C(t)');
grid on;


%-------------------------------------------------------------------------------------------
%                         VARIAS DOSIS
%-------------------------------------------------------------------------------------------



% Parámetros del modelo
C0 = 100; % Concentración inicial del medicamento (mg/L) inmediatamente después de la dosis
k = 0.1; % Constante de eliminación (1/horas)
dosisIntervalo = 40; % Intervalo de tiempo entre dosis en horas

% Tiempo
tFinal = 200; % Duración de la simulación en horas
dt = 0.1; % Paso de tiempo en horas
t = 0:dt:tFinal; % Vector de tiempo
C = zeros(size(t)); % Inicializa el vector de concentración

% Aplica la dosis inicial
C(1) = C0;
tiempoDesdeUltimaDosis = 0; % Inicia el contador de tiempo desde la última dosis

% Simulación
for i = 2:length(t)
    % Incrementa el tiempo desde la última dosis
    tiempoDesdeUltimaDosis = tiempoDesdeUltimaDosis + dt;
    
    % Verifica si es el momento de administrar otra dosis
    if tiempoDesdeUltimaDosis >= dosisIntervalo
        C(i) = C(i-1) + C0; % Añade dosis
        tiempoDesdeUltimaDosis = 0; % Reinicia el contador de tiempo desde la última dosis
    else
        % Calcula la disminución de la concentración debido a la eliminación
        dCdt = -k * C(i-1);
        C(i) = C(i-1) + dCdt * dt;
    end
end

% Gráfico
figure;
plot(t, C, 'LineWidth', 2);
xlabel('Tiempo (horas)');
ylabel('Concentración del medicamento (mg/L)');
title('Dinámica de C(t) dosis regulares');
grid on;
