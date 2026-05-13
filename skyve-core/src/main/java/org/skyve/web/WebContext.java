package org.skyve.web;

import java.io.Serializable;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.MessageSeverity;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * 
 */
public interface WebContext extends Serializable {
	/**
	 * The name of the web session attribute representing the logged in user.
	 */
	public static final String USER_SESSION_ATTRIBUTE_NAME = "user";

	/**
	 * 
	 * @return
	 */
	public @Nonnull String getKey();
	
	/**
	 * 
	 * @param key
	 */
	public void setKey(@Nonnull String key);
	
	/**
	 * 
	 * @param bizId
	 * @return
	 */
	public @Nullable Bean getBean(String bizId);
	
	/**
	 * Return the bean currently under view or edit within this context.
	 * This can change when zooming or other navigation.
	 * Although the current bean can be null within the context's life cycle it is guaranteed to be defined
	 * during the requests.
	 * @return	The current bean for this context.
	 * @throws IllegalStateException	If the current bean is null.
	 */
	public @Nonnull Bean getCurrentBean()
	throws IllegalStateException;
	
	/**
	 * 
	 * @param currentBean
	 */
	public void setCurrentBean(@Nullable Bean currentBean);
	
	/**
	 * The context key and the current bizId smashed together.
	 * @return
	 */
	public @Nonnull String getWebId();
	
	/**
	 * 
	 * @return
	 */
	public @Nullable String getAction();
	
	/**
	 * 
	 * @param action
	 */
	public void setAction(@Nullable String action);

	// TODO - implement view push/pop/replace/parent refresh
	// This class should have methods to accomplish the following
	// push a new view - ie popup on client-side, or render on server-side stack
	// pop a view off
	// get an action to be able to refresh its parent (what about refresh the parent's parent)
	// get a view to change its binding?
	// does an edit view needs its list view as a parent?
	// should the state of the views (ie history) be available for server-side interrogation?
	
	/**
	 * Add a message to the current view to be displayed.
	 * @param severity
	 * @param message
	 */
	public void message(@Nonnull MessageSeverity severity, @Nonnull String message);
	
	/**
	 * Add a growl (toast) to the current view to be popped.
	 * @param severity
	 * @param message
	 */
	public void growl(@Nonnull MessageSeverity severity, @Nonnull String message);
	
	/**
	 * Put this conversation into the conversation cache.
	 * @throws Exception
	 */
	public void cacheConversation() throws Exception;
	
	/**
	 * Kick off a new background task backed by this conversation.
	 * @param taskClass	The class of the task to execute.
	 * @throws Exception
	 */
	public <T extends Bean> void background(@Nonnull Class<? extends BackgroundTask<T>> taskClass) throws Exception;
	
	/**
	 * Kick off a new background task backed by this conversation without caching the conversation first.
	 * @param taskClass	The class of the task to execute.
	 * @throws Exception
	 */
	public <T extends Bean> void backgroundWithoutCachingConversation(@Nonnull Class<? extends BackgroundTask<T>> taskClass) throws Exception;
}
