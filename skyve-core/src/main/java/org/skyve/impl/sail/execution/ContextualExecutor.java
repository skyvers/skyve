package org.skyve.impl.sail.execution;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Map;
import java.util.TreeMap;

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

	/**
	 * The start of a variable token, {@code {var:name}}, that SAIL authors write in value-like attributes
	 * to use a value captured earlier in the run.
	 */
	public static final String VARIABLE_TOKEN_START = "{var:";

	private String runToken = null;

	/**
	 * The variables captured in this run, by name. Sorted so that a failure lists the defined names in order.
	 */
	private final Map<String, String> variables = new TreeMap<>();

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
		initialiseAutomation(automation);
	}

	/**
	 * Sets the automation-wide defaults (UX/UI, user agent type and test strategy) from the automation.
	 * Executors that drive the automation themselves, instead of calling {@link #executeAutomation(Automation)}
	 * on this class, call this first.
	 *
	 * @param automation	the automation being executed
	 */
	protected final void initialiseAutomation(@Nonnull Automation automation) {
		currentUxUi = automation.getUxui();
		currentUserAgentType = automation.getUserAgentType();
		testStrategy = automation.getTestStrategy();
		runToken = newRunToken();
		variables.clear();
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
	
	/**
	 * Returns the UX/UI the automation (or the latest context that names one) runs in.
	 *
	 * @return the UX/UI name, or null before the automation starts
	 */
	public @Nullable String getCurrentUxUi() {
		return currentUxUi;
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
	 * Returns {@code value} with every {@value #RUN_TOKEN} replaced by {@link #getRunToken()}, and every
	 * {@code {var:name}} replaced by the value of the variable {@code name}; see {@link #putVariable(String, String)}.
	 *
	 * <p>Executors must call this on every value-like attribute before using it; the
	 * attributes are listed on {@link Executor}. Generator executors that emit code
	 * for a later run would freeze the token into the generated code.
	 *
	 * <p>The value is scanned once, left to right, so a substituted value is not itself substituted.
	 *
	 * <p>Complexity: O(n) in the length of {@code value}.
	 *
	 * @param value the attribute value as written in the script
	 * @return the value with the tokens substituted; {@code value} itself when it has no token
	 * @throws MetaDataException if a {@code {var:name}} names a variable that has not been captured in this run,
	 * 			or a variable token has no closing brace
	 */
	public final @Nonnull String substituteTokens(@Nonnull String value) {
		int brace = value.indexOf('{');
		if (brace < 0) {
			return value;
		}
		StringBuilder result = null;
		int copied = 0;
		while (brace >= 0) {
			String replacement = null;
			int end = brace;
			if (value.startsWith(RUN_TOKEN, brace)) {
				replacement = getRunToken();
				end = brace + RUN_TOKEN.length();
			}
			else if (value.startsWith(VARIABLE_TOKEN_START, brace)) {
				int close = value.indexOf('}', brace);
				if (close < 0) {
					throw new MetaDataException("'" + value + "' has a " + VARIABLE_TOKEN_START + " with no closing }");
				}
				String name = value.substring(brace + VARIABLE_TOKEN_START.length(), close);
				replacement = variables.get(name);
				if (replacement == null) {
					throw new MetaDataException(VARIABLE_TOKEN_START + name + "} is not defined: capture it first with <captureValue variable=\"" +
													name + "\" />" + (variables.isEmpty() ? "; no variables are defined" : "; the variables defined are " + variables.keySet()));
				}
				end = close + 1;
			}
			if (replacement != null) {
				if (result == null) {
					result = new StringBuilder(value.length() + 32);
				}
				result.append(value, copied, brace).append(replacement);
				copied = end;
			}
			brace = value.indexOf('{', Math.max(end, brace + 1));
		}
		if (result == null) {
			return value;
		}
		return result.append(value, copied, value.length()).toString();
	}

	/**
	 * Sets the variable {@code name} to {@code value} for the rest of this run, replacing any earlier value.
	 * Each {@link #executeAutomation(Automation)} starts with no variables.
	 *
	 * <p>Side effects: later {@link #substituteTokens(String)} calls replace {@code {var:name}} with {@code value}.
	 *
	 * @param name the variable name
	 * @param value the value
	 */
	public final void putVariable(@Nonnull String name, @Nonnull String value) {
		variables.put(name, value);
	}

	/**
	 * Returns the value of the variable {@code name}.
	 *
	 * @param name the variable name
	 * @return the value, or {@code null} if the variable has not been captured in this run
	 */
	public final @Nullable String getVariable(@Nonnull String name) {
		return variables.get(name);
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
