package modules.admin.Dashboard.chain;

import modules.admin.Dashboard.chain.processors.DefaultWidgetProcessor;
import modules.admin.Dashboard.chain.processors.ViewCreationProcessor;
import modules.admin.Dashboard.chain.processors.DocumentModuleCreationProcessor;
import modules.admin.Dashboard.chain.processors.RepositoryApplicationProcessor;

/**
 * Factory class responsible for creating and configuring the Chain of Responsibility
 * for dashboard processing. Provides pre-configured chains for different scenarios.
 */
public class DashboardProcessorChainFactory {
    
    /**
     * Creates a processor chain for loading user dashboards.
     * Chain: DefaultWidgets -> ViewCreation -> RepositoryApplication
     */
    public static DashboardProcessor createLoadDashboardChain() {
        DefaultWidgetProcessor defaultWidgetProcessor = new DefaultWidgetProcessor();
        ViewCreationProcessor viewCreationProcessor = new ViewCreationProcessor();
        RepositoryApplicationProcessor repositoryApplicationProcessor = new RepositoryApplicationProcessor();
        
        return chainProcessors(defaultWidgetProcessor, viewCreationProcessor, repositoryApplicationProcessor);
    }
    
    /**
     * Creates a processor chain for activating dashboards.
     * Chain: ViewCreation -> DocumentModuleCreation -> RepositoryApplication
     */
    public static DashboardProcessor createActivateDashboardChain() {
        ViewCreationProcessor viewCreationProcessor = new ViewCreationProcessor();
        DocumentModuleCreationProcessor documentModuleCreationProcessor = new DocumentModuleCreationProcessor();
        RepositoryApplicationProcessor repositoryApplicationProcessor = new RepositoryApplicationProcessor();
        
        return chainProcessors(viewCreationProcessor, documentModuleCreationProcessor, repositoryApplicationProcessor);
    }
    
    /**
     * Helper method to chain processors together.
     */
    private static DashboardProcessor chainProcessors(DashboardProcessor... processors) {
        if (processors == null || processors.length == 0) {
            return null;
        }
        
        DashboardProcessor first = processors[0];
        DashboardProcessor current = first;
        
        for (int i = 1; i < processors.length; i++) {
            current = current.setNext(processors[i]);
        }
        
        return first;
    }
}
