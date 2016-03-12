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
	
	/**
	 * Append a balanced folder structure for storing a content file based on it's content ID.
	 * 
	 * @param id The content ID
	 * @param pathToAppendTo	The path to append to.
	 */
	public static void appendBalancedFolderPathFromContentId(String id, StringBuilder pathToAppendTo) {
		pathToAppendTo.append(id.substring(5, 7)).append('/');
		pathToAppendTo.append(id.substring(10, 12)).append('/');
		pathToAppendTo.append(id.substring(15, 17)).append('/');
	}
}
