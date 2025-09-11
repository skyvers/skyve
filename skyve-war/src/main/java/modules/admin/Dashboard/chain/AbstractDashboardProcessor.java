package modules.admin.Dashboard.chain;

import java.lang.reflect.Field;

/**
 * Abstract base class for dashboard processors implementing the Chain of Responsibility pattern.
 * Provides common functionality for chaining processors together.
 */
public abstract class AbstractDashboardProcessor implements DashboardProcessor {
    
    private DashboardProcessor nextProcessor;
    
    @Override
    public DashboardProcessor setNext(DashboardProcessor nextProcessor) {
        this.nextProcessor = nextProcessor;
        return nextProcessor;
    }
    
    @Override
    public DashboardProcessingContext process(DashboardProcessingContext context) {
        if (canHandle(context)) {
            return doProcess(context);
        } else if (nextProcessor != null) {
            return nextProcessor.process(context);
        }
        return context;
    }
    
    /**
     * Template method that subclasses must implement to perform their specific processing.
     * 
     * @param context The dashboard processing context
     * @return The updated context after processing
     */
    protected abstract DashboardProcessingContext doProcess(DashboardProcessingContext context);
    
    /**
     * Utility method to pass processing to the next processor in the chain.
     * 
     * @param context The context to pass along
     * @return The result from the next processor, or the unchanged context if no next processor
     */
    protected DashboardProcessingContext passToNext(DashboardProcessingContext context) {
        if (nextProcessor != null) {
            return nextProcessor.process(context);
        }
        return context;
    }
    
    /**
	 * Utility method to find a field in a class hierarchy by traversing up through
	 * all superclasses
	 * 
	 * @param clazz The starting class to search from
	 * @param fieldName The name of the field to find
	 * @return The Field if found, null otherwise
	 */
	protected static Field findFieldInHierarchy(Class<?> clazz, String fieldName) {
		Class<?> currentClass = clazz;
		while (currentClass != null) {
			try {
				Field field = currentClass.getDeclaredField(fieldName);
				return field;
			} catch (@SuppressWarnings("unused") NoSuchFieldException e) {
				currentClass = currentClass.getSuperclass();
			}
		}
		return null; // Field not found in hierarchy
	}
}
