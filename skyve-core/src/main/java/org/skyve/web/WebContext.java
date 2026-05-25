package org.skyve.web;

import java.io.Serializable;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.MessageSeverity;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Per-conversation server-side context for view rendering and action execution.
 *
 * <p>A {@code WebContext} represents a single conversation — typically one browser tab
 * or popup window — and holds the conversation's current state: the current bean under
 * edit, any pending messages or growls, and the cached bean map indexed by {@code bizId}.
 *
 * <p>Actions that need to display messages, trigger background work, or access the
 * current conversation bean receive a {@code WebContext} via
 * {@link org.skyve.metadata.controller.ServerSideAction#execute(org.skyve.domain.Bean, WebContext)}.
 *
 * <p>Threading: a {@code WebContext} instance is confined to a single HTTP request
 * thread at a time. The framework ensures this via the conversation cache mechanism
 * ({@link #cacheConversation()}). Never share a {@code WebContext} across threads.
 *
 * @see BackgroundTask
 * @see org.skyve.metadata.controller.ServerSideAction
 */
public interface WebContext extends Serializable {
	/**
	 * The name of the web session attribute representing the logged in user.
	 */
	public static final String USER_SESSION_ATTRIBUTE_NAME = "user";

	/**
	 * Returns the unique conversation key that identifies this context in the conversation cache.
	 * The key is stable for the lifetime of the conversation.
	 */
	public @Nonnull String getKey();
	
	/**
	 * Sets the conversation key.
	 *
	 * @param key the unique cache key; must not be {@code null}
	 */
	public void setKey(@Nonnull String key);
	
	/**
	 * Returns the bean with the given {@code bizId} from this conversation's bean map,
	 * or {@code null} if no such bean exists in this conversation.
	 *
	 * @param bizId the {@link org.skyve.domain.Bean#getBizId() bizId} to look up
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
	 * Sets the current bean under view or edit.
	 *
	 * @param currentBean the bean to make current; may be {@code null} during transitions
	 */
	public void setCurrentBean(@Nullable Bean currentBean);
	
	/**
	 * The context key and the current bizId smashed together.
	 * @return
	 */
	public @Nonnull String getWebId();
	
	/**
	 * Returns the action binding name for the current request, or {@code null} if none is set.
	 */
	public @Nullable String getAction();
	
	/**
	 * Sets the action binding name for the current request.
	 *
	 * @param action the action name, or {@code null} to clear
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
