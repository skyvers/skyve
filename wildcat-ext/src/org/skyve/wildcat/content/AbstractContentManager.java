package org.skyve.wildcat.content;

public abstract class AbstractContentManager implements ContentManager {
	public static Class<? extends AbstractContentManager> IMPLEMENTATION_CLASS;
	
	public static AbstractContentManager get() {
		try {
			AbstractContentManager result = IMPLEMENTATION_CLASS.newInstance();
			return result;
		}
		catch (Exception e) {
			throw new IllegalArgumentException(IMPLEMENTATION_CLASS + " was not a good choice.", e);
		}
	}
	
	public abstract void init() throws Exception;
	public abstract void dispose() throws Exception;
}
