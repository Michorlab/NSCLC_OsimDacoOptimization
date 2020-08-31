clear; close all;
% compiling the system to make sure the 
% latest changes have been committed. 
build_system

% setting up paths
provision_workspace;

% pulling out the system information
cfg  = auto_fetch_system_information();

% set name                  | Description
% -------------------------------------------------------
% default                   | default

% selecting the default parameter set:
cfg  = system_select_set(cfg, 'default');


% fetching the parameter values
parameters = system_fetch_parameters(cfg);

% The previous statement sets 'parameters' to the values 
% in the currently selected parameter set. To overwrite 
% a specific parameter uncomment the following statement 
% and replace PNAME with the name of the parameter 
% and VALUE with the desired value:
%
% parameters = system_set_parameter(cfg, parameters, 'PNAME', VALUE);

% %cfg=system_zero_inputs(cfg);
% cfg=system_set_bolus(cfg, 'C1o', ...  % 
%                            [ 0.0], ...  % hour
%                            [ 0.0]);     % mg/L 
 
% cfg=system_set_bolus(cfg, 'C2o', ...  % 
%                            [ 0.0], ...  % hour
%                            [ 0.0]);     % mg/L 
 
% cfg=system_set_bolus(cfg, 'C2d', ...  % 
%                            [ 0.0], ...  % hour
%                            [ 0.0]);     % mg/L 
 
% cfg=system_set_bolus(cfg, 'C3d', ...  % 
%                            [ 0.0], ...  % hour
%                            [ 0.0]);     % mg/L 
 
% cfg=system_set_bolus(cfg, 'Ato', ...  % 
%                            [ 0.0], ...  % hour
%                            [80.0]);     % mg 
 
% cfg=system_set_bolus(cfg, 'Atd', ...  % 
%                            [ 0.0], ...  % hour
%                            [45.0]);     % mg 
 


% No infusion rates defined


% No covariates found



% Evaluate system at the following output times
cfg = system_set_option(cfg, 'simulation', 'output_times', linspace(0, 10, (10-0)/.1)');


% Options to control simulation execution and outputs are defined here.
% To see a list of options type:
% help system_set_option
%
% Here are a few useful options:
% To force matlab to use simulink uncomment the following line:
% cfg = system_set_option(cfg, 'simulation', 'integrate_with', 'simulink');
%
% To change the ODE solver that is used:
% cfg = system_set_option(cfg, 'solver', 'solver', 'ode23s');


% -----------------------------------------------------------------------
% Indiviudal Simulation:
% Simulating the system and storing the result in  som (Simulation Output
% Mapped) -- a data structure with times, states and outputs mapped to 
% their internal names
som = run_simulation_ubiquity(parameters, cfg);


% the following just plots all of the 
% specified outputs
figure(1);
hold on;

% You can access different parts of the
% simulation using the som variable.
% som.times.TIMESCALE 
% som.outputs.OUTPUTNAME 
% som.states.STATENAME 
plot(som.times.sim_time, ...
     som.outputs.C_osi, 'b-'); 


prepare_figure('present');
% set(gca, 'yscale', 'log');
  xlabel('time');
  ylabel('outputs');
% -----------------------------------------------------------------------



% -----------------------------------------------------------------------
% Stochastic Simulation
% predictions = simulate_subjects(parameters, cfg)
% mc = fetch_color_codes;

% % Uncomment the lines below and substiute the          
% % desired timescale and output for TS and OUTPUT       
% patch(predictions.times_patch.TS, ...                  
%       predictions.outputs_patch.OUTPUT.ci, ...         
%       mc.light_blue, 'edgecolor', 'none');           

% % This plots the mean:                                 
%   plot(predictions.times.weeks, ...                    
%        predictions.outputs_stats.Cp_Total.mean, 'b-'); 

% -----------------------------------------------------------------------

