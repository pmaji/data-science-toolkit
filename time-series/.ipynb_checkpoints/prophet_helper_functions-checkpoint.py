from matplotlib import pyplot as plt

def dt_restricted_prophet_plt(
    m, fcst, visible_window_start_dt=None, visible_window_end_dt=None, ax=None,
    uncertainty=True, plot_cap=True, xlabel='ds', ylabel='y', figsize=(10,6)
):
    """
    Plot the Prophet forecast, restricting the plot using optional start and end dates.
    If no start or end dates are selected, the default behavior is like Prophet.plot.
    
    Parameters
    ----------
    m: Prophet model.
    fcst: pd.DataFrame output of m.predict.
    visible_window_start_dt: Optional first date to include in the plot.
    visible_window_end_dt: Optional last date to include in the plot. 
    ax: Optional matplotlib axes on which to plot.
    uncertainty: Optional boolean to plot uncertainty intervals.
    plot_cap: Optional boolean indicating if the capacity should be shown
        in the figure, if available.
    xlabel: Optional label name on X-axis.
    ylabel: Optional label name on Y-axis.
    figsize: Optional tuple width, height in inches.
    
    Returns
    -------
    A matplotlib figure.
    """
    
    # checking if the user chose to restrict the date range shown
    if visible_window_start_dt is not None:
        functional_window_start_dt = visible_window_start_dt
    else:
        functional_window_start_dt = fcst['ds'].min()
        
    if visible_window_end_dt is not None:
        functional_window_end_dt = visible_window_end_dt
    else:
        functional_window_end_dt = fcst['ds'].max()
        
    # subsets the history & forecast to plot (if the user provided start or end dates)
    visible_history = m.history.loc[(m.history['ds'] >= functional_window_start_dt) &
                                    (m.history['ds'] <= functional_window_end_dt),:]
    visible_forecast = forecast.loc[(forecast['ds'] >= functional_window_start_dt) &
                                    (forecast['ds'] <= functional_window_end_dt),:]
    
    # build the matplotlib figure
    if ax is None:
        fig = plt.figure(facecolor='w', figsize=figsize)
        ax = fig.add_subplot(111)
    else:
        fig = ax.get_figure()
    fcst_t = visible_forecast['ds'].dt.to_pydatetime()
    ax.plot(visible_history['ds'].dt.to_pydatetime(), visible_history['y'], 'k.')
    ax.plot(fcst_t, visible_forecast['yhat'], ls='-', c='#0072B2')
    if 'cap' in visible_forecast and plot_cap:
        ax.plot(fcst_t, visible_forecast['cap'], ls='--', c='k')
    if m.logistic_floor and 'floor' in visible_forecast and plot_cap:
        ax.plot(fcst_t, visible_forecast['floor'], ls='--', c='k')
    if uncertainty:
        ax.fill_between(fcst_t, visible_forecast['yhat_lower'], visible_forecast['yhat_upper'],
                        color='#0072B2', alpha=0.2)
    ax.grid(True, which='major', c='gray', ls='-', lw=1, alpha=0.2)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    fig.tight_layout()
    
    return fig