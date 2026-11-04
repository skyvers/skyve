package org.skyve.impl.sail.execution;

import java.util.ArrayDeque;
import java.util.Deque;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateCalendar;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateLink;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMap;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateTree;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Base class for executors that maintain and manage a stack of {@link AutomationContext} instances
 * while running SAIL automation steps.
 * 
 * @author mike
 */
public abstract class ContextualExecutor<T extends AutomationContext> implements Executor {

	private String currentUxUi = null;
	private UserAgentType currentUserAgentType = null;
	private TestStrategy testStrategy = null;

	private Deque<T> contextStack = new ArrayDeque<>(8);

	/**
	 * The token that SAIL authors write in value-like attributes to make values unique to a run.
	 */
	public static final String RUN_TOKEN = "{run}";

	private String runToken = null;

	protected final void push(@Nonnull T context) {
		String uxui = context.getUxui();
		if (uxui == null) {
			context.setUxui(currentUxUi);
		} else {
			currentUxUi = uxui;
		}

		UserAgentType userAgentType = context.getUserAgentType();
		if (userAgentType == null) {
			context.setUserAgentType(currentUserAgentType);
		} else {
			currentUserAgentType = userAgentType;
		}

		contextStack.push(context);
	}

	protected final @Nonnull T pop() {
		T result = contextStack.pop();

		T current = contextStack.peek();
		if (current != null) {
			String uxui = current.getUxui();
			if (uxui != null) {
				currentUxUi = uxui;
			}

			UserAgentType userAgentType = current.getUserAgentType();
			if (userAgentType != null) {
				currentUserAgentType = userAgentType;
			}
		}

		return result;
	}
	
	protected final @Nullable T peek() {
		return contextStack.peek();
	}
	
	protected final void clear() {
		contextStack.clear();
	}

	/**
	 * Do nothing but populate the current automation context.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public void executeAutomation(@Nonnull Automation automation) {
		currentUxUi = automation.getUxui();
		currentUserAgentType = automation.getUserAgentType();
		testStrategy = automation.getTestStrategy();
		runToken = newRunToken();
	}
	
	/**
	 * Do nothing but populate the automation context.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public void executeNavigateList(@Nonnull NavigateList list) {
		T context = peek();
		if (context == null) {
			throw new MetaDataException("<" + list.getClass().getSimpleName() + " /> has no context to navigate in");
		}
		context.setViewType(ViewType.list);
		
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(list.getModuleName());
		String documentName = list.getDocumentName();
		String queryName = list.getQueryName();
		String modelName = list.getModelName();

		Document drivingDocument = null;
		if (queryName != null) {
			MetaDataQueryDefinition q = m.getMetaDataQuery(queryName);
			if (q == null) {
				// As the list grid does, an undefined query name is the name of a document to use the default query of
				q = m.getDocumentDefaultQuery(c, queryName);
			}
			drivingDocument = m.getDocument(c, q.getDocumentName());
		} else if (documentName != null) {
			drivingDocument = m.getDocument(c, documentName);
			if (modelName != null) {
				ListModel<Bean> model = drivingDocument.getListModel(c, modelName, false);
				drivingDocument = model.getDrivingDocument();
			}
		}

		if (drivingDocument != null) {
			context.setModuleName(drivingDocument.getOwningModuleName());
			context.setDocumentName(drivingDocument.getName());
		}
	}

	/**
	 * Do nothing but populate the automation context.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public void executeNavigateEdit(@Nonnull NavigateEdit edit) {
		T context = peek();
		if (context == null) {
			throw new MetaDataException("<NavigateEdit /> has no context to navigate in");
		}
		context.setViewType(ViewType.edit);
		context.setModuleName(edit.getModuleName());
		context.setDocumentName(edit.getDocumentName());
	}

	/**
	 * Do nothing but populate the automation context.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public void executeNavigateTree(@Nonnull NavigateTree tree) {
		executeNavigateList(tree);
	}

	/**
	 * Do nothing but populate the automation context.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public void executeNavigateMap(@Nonnull NavigateMap map) {
		executeNavigateList(map);
	}

	/**
	 * Do nothing but populate the automation context.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public void executeNavigateCalendar(@Nonnull NavigateCalendar calendar) {
		executeNavigateList(calendar);
	}
	
	/**
	 * Do nothing but clear the automation context.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public void executeNavigateLink(@Nonnull NavigateLink link) {
		T context = peek();
		if (context == null) {
			throw new MetaDataException("<NavigateLink /> has no context to navigate in");
		}
		context.setDocumentName(null);
		context.setViewType(null);
	}
	
	public @Nullable TestStrategy getTestStrategy() {
		return testStrategy;
	}
	
	public @Nonnull T newContext(@Nonnull PushListContext push, @Nonnull T newContext) {
		String moduleName = push.getModuleName();
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(moduleName);

		String documentName = push.getDocumentName();
		String queryName = push.getQueryName();
		String modelName = push.getModelName();
		
		if (queryName != null) {
			MetaDataQueryDefinition q = m.getMetaDataQuery(queryName);
			if (q == null) {
				// As the list grid does, an undefined query name is the name of a document to use the default query of
				q = m.getDocumentDefaultQuery(c, queryName);
			}

			m = q.getOwningModule();
			newContext.setModuleName(m.getName());
			newContext.setDocumentName(q.getDocumentName());
		} else if (documentName != null) {
			Document d = m.getDocument(c, documentName);
			if (modelName != null) {
				Document drivingDocument = d.getListModel(c, modelName, false).getDrivingDocument();
				if (drivingDocument == null) {
					throw new IllegalStateException("List model " + modelName + " of document " + moduleName + '.' + documentName +
														" has no driving document");
				}
				d = drivingDocument;
			} else {
				push.setQueryName(documentName);
			}

			newContext.setModuleName(d.getOwningModuleName());
			newContext.setDocumentName(d.getName());
		} else {
			throw new MetaDataException("NavigateList must have module and one of (query, document, document & model)");
		}

		newContext.setViewType(ViewType.list);
		newContext.setUxui(push.getUxui());
		newContext.setUserAgentType(push.getUserAgentType());
		
		return newContext;
	}
	
	public @Nonnull T newContext(@Nonnull PushEditContext push, @Nonnull T newContext) {
		newContext.setModuleName(push.getModuleName());
		newContext.setDocumentName(push.getDocumentName());

		if (Boolean.TRUE.equals(push.getCreateView())) {
			newContext.setViewType(ViewType.create);
		} else {
			newContext.setViewType(ViewType.edit);
		}

		newContext.setUxui(push.getUxui());
		newContext.setUserAgentType(push.getUserAgentType());

		return newContext;
	}

	/**
	 * Returns the token that replaces {@value #RUN_TOKEN} in this run's values.
	 *
	 * <p>A new token is made by each {@link #executeAutomation(Automation)}, and on first use if
	 * steps are executed without one. It is 8 or more lower-case letters and digits, derived
	 * from the current time, so reruns against the same database do not clash on unique keys
	 * such as user and group names.
	 *
	 * @return the run token
	 */
	public final @Nonnull String getRunToken() {
		String result = runToken;
		if (result == null) {
			result = newRunToken();
			runToken = result;
		}
		return result;
	}

	/**
	 * Returns {@code value} with every {@value #RUN_TOKEN} replaced by {@link #getRunToken()}.
	 *
	 * <p>Executors must call this on every value-like attribute before using it; the
	 * attributes are listed on {@link Executor}. Generator executors that emit code
	 * for a later run would freeze the token into the generated code.
	 *
	 * @param value the attribute value as written in the script
	 * @return the value with the run token substituted; {@code value} itself when it has no {@value #RUN_TOKEN}
	 */
	public final @Nonnull String substituteRunToken(@Nonnull String value) {
		if (value.contains(RUN_TOKEN)) {
			return value.replace(RUN_TOKEN, getRunToken());
		}
		return value;
	}

	/**
	 * Makes a run token from the current time in base 36.
	 *
	 * @return a new run token
	 */
	private static @Nonnull String newRunToken() {
		return Long.toString(System.currentTimeMillis(), Character.MAX_RADIX);
	}
}
