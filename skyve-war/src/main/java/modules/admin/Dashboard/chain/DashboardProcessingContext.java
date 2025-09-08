package modules.admin.Dashboard.chain;

import modules.admin.Dashboard.DashboardExtension;
import modules.admin.Dashboard.chain.services.FavouritesService;
import org.skyve.metadata.view.fluent.FluentView;
import org.skyve.metadata.model.document.fluent.FluentDocument;
import org.skyve.metadata.module.fluent.FluentModule;
import org.skyve.metadata.module.Module;
import org.skyve.impl.metadata.repository.DefaultRepository;

/**
 * Context object that carries data through the Chain of Responsibility for dashboard processing.
 * This object accumulates state as it moves through the chain.
 */
public class DashboardProcessingContext {
    
    public enum ProcessingType {
        LOAD_DASHBOARD,
        ACTIVATE_DASHBOARD
    }
    
    private final DashboardExtension dashboard;
    private final ProcessingType processingType;
    private final DefaultRepository repository;
    
    // Injected services
    private FavouritesService favouritesService;
    
    // Results accumulated during processing
    private FluentView fluentView;
    private FluentDocument fluentDocument;
    private FluentModule fluentModule;
    private Module module;
    private boolean processed = false;
    private String errorMessage;
    private int customChartCount = 0;
    
    public DashboardProcessingContext(DashboardExtension dashboard, ProcessingType processingType, DefaultRepository repository) {
        this.dashboard = dashboard;
        this.processingType = processingType;
        this.repository = repository;
    }
    
    // Getters
    public DashboardExtension getDashboard() {
        return dashboard;
    }
    
    public ProcessingType getProcessingType() {
        return processingType;
    }
    
    public DefaultRepository getRepository() {
        return repository;
    }
    
    public FluentView getFluentView() {
        return fluentView;
    }
    
    public FluentDocument getFluentDocument() {
        return fluentDocument;
    }
    
    public FluentModule getFluentModule() {
        return fluentModule;
    }
    
    public Module getModule() {
        return module;
    }
    
    public boolean isProcessed() {
        return processed;
    }
    
    public String getErrorMessage() {
        return errorMessage;
    }
    
    public int getCustomChartCount() {
        return customChartCount;
    }
    
    public FavouritesService getFavouritesService() {
        return favouritesService;
    }
    
    // Setters
    public void setFluentView(FluentView fluentView) {
        this.fluentView = fluentView;
    }
    
    public void setFluentDocument(FluentDocument fluentDocument) {
        this.fluentDocument = fluentDocument;
    }
    
    public void setFluentModule(FluentModule fluentModule) {
        this.fluentModule = fluentModule;
    }
    
    public void setModule(Module module) {
        this.module = module;
    }
    
    public void setProcessed(boolean processed) {
        this.processed = processed;
    }
    
    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }
    
    public void setCustomChartCount(int customChartCount) {
        this.customChartCount = customChartCount;
    }
    
    public void setFavouritesService(FavouritesService favouritesService) {
        this.favouritesService = favouritesService;
    }
    
    // Utility methods
    public boolean isLoadDashboard() {
        return ProcessingType.LOAD_DASHBOARD.equals(processingType);
    }
    
    public boolean isActivateDashboard() {
        return ProcessingType.ACTIVATE_DASHBOARD.equals(processingType);
    }
    
    public boolean hasError() {
        return errorMessage != null && !errorMessage.trim().isEmpty();
    }
}
