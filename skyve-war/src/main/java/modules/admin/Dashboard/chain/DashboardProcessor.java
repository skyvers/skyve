package modules.admin.Dashboard.chain;

/**
 * Interface for the Chain of Responsibility pattern used in Dashboard processing.
 * Each processor in the chain handles a specific aspect of dashboard creation/loading.
 */
public interface DashboardProcessor {
    
    /**
     * Sets the next processor in the chain.
     * 
     * @param nextProcessor The next processor to be called if this processor cannot handle the request
     * @return This processor for method chaining
     */
    DashboardProcessor setNext(DashboardProcessor nextProcessor);
    
    /**
     * Processes the dashboard request. Each processor should either handle the request
     * or pass it to the next processor in the chain.
     * 
     * @param context The dashboard processing context containing all necessary data
     * @return The updated context after processing, or null if processing failed
     */
    DashboardProcessingContext process(DashboardProcessingContext context);
    
    /**
     * Determines if this processor can handle the current request.
     * 
     * @param context The dashboard processing context
     * @return true if this processor can handle the request, false otherwise
     */
    boolean canHandle(DashboardProcessingContext context);
}
