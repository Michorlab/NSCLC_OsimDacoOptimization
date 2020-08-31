clear; close all;

% flowctl - Controls the flow of the analysis script
%{
flowctl = 'estimate';
flowctl = 'plot previous estimate';
flowctl = 'previous estimate as guess';
%}
flowctl = 'plot guess';
analysis_name = 'ANAME';

% Compiling the system to make sure the 
% latest changes have been committed. 
build_system

% Setting up paths
provision_workspace;

% Defining color codes to be used 
% in plotting below
mc = fetch_color_codes;

% Pulling out the system information
cfg = auto_fetch_system_information;

% set name                  | Description
% -------------------------------------------------------
% default                   | default

% Select the parameter set and the parameters to estimate:
%{
% The following will estimate a subset of the parameters:
pnames ={ 'PNAME1'
          'PNAME2' 
          'PNAME3' }; 
cfg  = system_select_set(cfg, 'default', pnames);

% This will estimate all of the parameters:
cfg  = system_select_set(cfg, 'default');
%}


%{
% To overwrite the initial guess at the scripting level for the parameter
% PNAME with the value VALUE use the following:
cfg = system_set_guess(cfg, 'PNAME', VALUE);
%}

% Specifying Simulation and Estimation options
%
% help system_set_option
%{

% Simulation options:
% If you have simulink you should use it:
cfg = system_set_option(cfg, 'simulation', 'integrate_with', 'simulink');

% Set the output times for smooth profiles when plotting. This will be the
% default output times unless overwritten at the cohort level:
cfg = system_set_option(cfg, 'simulation', ...
                             'output_times', ...
                              linspace(0, 10, (10-0)/.1)');
% Solver options
cfg = system_set_option(cfg, 'solver',     'RelTol',  1e-5);
cfg = system_set_option(cfg, 'solver',     'AbsTol',  1e-9);
cfg = system_set_option(cfg, 'solver',     'solver', 'ode23s');

% Estimation options:
% % Monitoring the estimation process:
cfg = system_set_option(cfg, 'estimation', 'monitor_iteration_history', 100);
cfg = system_set_option(cfg, 'estimation', 'monitor_slope_tolerance',   0.001);
cfg = system_set_option(cfg, 'estimation', 'monitor_exit_when_stable', 'yes');

% To disable the monitoring set the status function to ''
% cfg = system_set_option(cfg, 'estimation', 'monitor_status_function', '');
%
% % This disables the monitoring and performs a sort of simulated annealing:
cfg = system_set_option(cfg, 'estimation', 'effort', 100);
%
% % Overwrite optimization options:
% To use the Nelder-Mead method:
cfg = system_set_option(cfg, 'estimation', 'optimizer', 'fminsearch');
cfg = system_set_option(cfg, 'estimation', 'optimization_options', ...
              optimset('Display',   'iter', ...
                       'TolFun',     1e-3,  ...
                       'MaxIter',    3000,  ...
                       'MaxFunEval', 3000));

% If you have the Global Optimization Toolbox:
% To use the Genetic Algorithm
cfg = system_set_option(cfg, 'estimation', 'optimizer', 'ga');
cfg = system_set_option(cfg, 'estimation', 'optimization_options', ...
             gaoptimset('Display',      'iter', ...
                        'MaxGenerations', 500)); 

%}

% Loading datasets   
%
% help system_load_dataset
%{
% From Excel sheet
cfg = system_load_dataset(cfg, 'DSNAME'     , ...
                         sprintf('data%sDSFILE.xls', filesep), ...
                         'SHEETNAME');
% From csv        
cfg = system_load_dataset(cfg, 'DSNAME'     , ...
                         sprintf('data%sDSFILE.csv', filesep))
% From tab        
cfg = system_load_dataset(cfg, 'DSNAME'     , ...
                         sprintf('data%sDSFILE.tab', filesep))
%}


% Defining the cohorts
%
% Clearing all of the cohorts
cfg = system_clear_cohorts(cfg);
 
% One entry for each cohort:
% For more information type:
%
% help system_define_cohort
%
% It is necessary to replace the following compontents:
%
% CHNAME    - cohort name
% COLNAME   - column name in dataset
% ONAME     - output name
% TIMECOL   - column name in dataset with the observation times
% TS        - model timescale corresponding to TIMECOL
% OBSCOL    - column name in dataset with the observation values
% MODOUTPUT - model output corresponding to OBSCOL
%
% Only specify inputs that are non-zero. Simply ignore inputs that do not
% exist for the given cohort.
%{
clear cohort;
cohort.name                                 = 'CHNAME';
cohort.cf.COLNAME                           = [];
cohort.cf.COLNAME                           = [];
cohort.dataset                              = 'DSNAME';
cohort.inputs.bolus.C1o.TIME                = [];
cohort.inputs.bolus.C1o.AMT                 = [];
cohort.inputs.bolus.C2o.TIME                = [];
cohort.inputs.bolus.C2o.AMT                 = [];
cohort.inputs.bolus.C2d.TIME                = [];
cohort.inputs.bolus.C2d.AMT                 = [];
cohort.inputs.bolus.C3d.TIME                = [];
cohort.inputs.bolus.C3d.AMT                 = [];
cohort.inputs.bolus.Ato.TIME                = [];
cohort.inputs.bolus.Ato.AMT                 = [];
cohort.inputs.bolus.Atd.TIME                = [];
cohort.inputs.bolus.Atd.AMT                 = [];
 
cohort.outputs.ONAME.of.COLNAME             = [];
cohort.outputs.ONAME.of.COLNAME             = [];
cohort.outputs.ONAME.obs.time               = 'TIMECOL'; 
cohort.outputs.ONAME.obs.value              = 'OBSCOL';
cohort.outputs.ONAME.model.time             = 'TS';        
cohort.outputs.ONAME.model.value            = 'MODOUTPUT';  
cohort.outputs.ONAME.model.variance         = '1';  
% Optional
cohort.outputs.ONAME.options.marker_color   = 'r';
cohort.outputs.ONAME.options.marker_shape   = 'o';
cohort.outputs.ONAME.options.marker_line    = '-'; 

% Adding the cohort:
cfg = system_define_cohort(cfg, cohort);
%}

% Getting the parameters either by estimation, loading the previous estimation
% results, or pulling out the initial guess
%
% The variable pest will be defined here and used below for running
% simulations:
pnames_est = cfg.estimation.parameters.names;
if(strcmp(flowctl , 'estimate') | strcmp(flowctl, 'previous estimate as guess'))

  % Checking the analysis name
  name_check = ubiquity_name_check(analysis_name)   ;
  if(~name_check.isgood)
    vp(cfg, sprintf('Error: the analyssis name >%s< is invalid', analysis_name  ))
    vp(cfg, sprintf('Problems: %s', name_check.msg))
    analysis_name   = 'analysis';
    vp(cfg, sprintf('Instead Using: %s', analysis_name  ))
  end

  % Loading the previous estimate
  if(strcmp(flowctl, 'previous estimate as guess'))
    eval(sprintf('load output%sanalysis_%s.mat pest ss pnames_est',filesep, analysis_name))
    cfg.estimation.parameters.guess = pest;
  end

  % Performing the parameter estimation: 
  [pest, ss] = estimate_parameters(cfg);

  % Saving the results:
  eval(sprintf('save output%sanalysis_%s.mat pest ss pnames_est',filesep, analysis_name))
  archive_estimation(analysis_name, cfg);
elseif(strcmp(flowctl , 'plot guess'))
  pest   = system_fetch_guess(cfg);
else 
  % Loading the previously saved results:
  eval(sprintf('load output%sanalysis_%s.mat pest ss pnames_est',filesep, analysis_name))
end


% Cohorts defined here will be plotted 
% but not used during the estimation

% Running simulation for plotting
%
[erp] = system_simulate_estimation_results(pest, cfg);

% Plotting the observations and predictions
%
plot_opts = [];
%{
% save figures to file with analysis_name prefix
plot_opts.save_figs = analysis_name;

% For each output the following can be defined
% to control plotting.  See help system_plot_cohorts 
% for more options and details.
plot_opts.outputs.ONAME.panels   = 'no';
plot_opts.outputs.ONAME.rows     = 1;
plot_opts.outputs.ONAME.cols     = 2;
plot_opts.outputs.ONAME.yscale   = 'log';
plot_opts.outputs.ONAME.ylim     = [1,    100];
plot_opts.outputs.ONAME.xlim     = [0,    100];
plot_opts.outputs.ONAME.ylabel   = 'Output (units)';
plot_opts.outputs.ONAME.xlabel   = 'Time (units)';
plot_opts.outputs.ONAME.datatype = 'all';
%}



close all;
system_plot_cohorts(erp, plot_opts, cfg);

