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

public abstract class ContextualExecutor<T extends AutomationContext> implements Executor {
	private String currentUxUi = null;
	private UserAgentType currentUserAgentType = null;
	private TestStrategy testStrategy = null;
	
	private Deque<T> contextStack = new ArrayDeque<>(8);

	protected final void push(T context) {
		String uxui = context.getUxui();
		if (uxui == null) {
			context.setUxui(currentUxUi);
		}
		else {
			currentUxUi = uxui;
		}
		UserAgentType userAgentType = context.getUserAgentType();
		if (userAgentType == null) {
			context.setUserAgentType(currentUserAgentType);
		}
		else {
			currentUserAgentType = userAgentType;
		}
		contextStack.push(context);
	}

	protected final T pop() {
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
	
	protected final T peek() {
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
	public void executeAutomation(Automation automation) {
		currentUxUi = automation.getUxui();
		currentUserAgentType = automation.getUserAgentType();
		testStrategy = automation.getTestStrategy();
	}
	
	/**
	 * Do nothing but populate the automation context.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public void executeNavigateList(NavigateList list) {
		T context = peek();
		context.setViewType(ViewType.list);
		
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(list.getModuleName());
		String documentName = list.getDocumentName();
		String queryName = list.getQueryName();
		String modelName = list.getModelName();

		Document drivingDocument = null;
		if (queryName != null) {
			drivingDocument = m.getDocument(c, m.getNullSafeMetaDataQuery(queryName).getDocumentName());
		}
		else if (documentName != null) {
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
	public void executeNavigateEdit(NavigateEdit edit) {
		T context = peek();
		context.setViewType(ViewType.edit);
		context.setModuleName(edit.getModuleName());
		context.setDocumentName(edit.getDocumentName());
	}

	/**
	 * Do nothing but populate the automation context.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public void executeNavigateTree(NavigateTree tree) {
		executeNavigateList(tree);
	}

	/**
	 * Do nothing but populate the automation context.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public void executeNavigateMap(NavigateMap map) {
		executeNavigateList(map);
	}

	/**
	 * Do nothing but populate the automation context.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public void executeNavigateCalendar(NavigateCalendar calendar) {
		executeNavigateList(calendar);
	}
	
	/**
	 * Do nothing but clear the automation context.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public void executeNavigateLink(NavigateLink link) {
		T context = peek();
		context.setDocumentName(null);
		context.setViewType(null);
	}
	
	public TestStrategy getTestStrategy() {
		return testStrategy;
	}
	
	public T newContext(PushListContext push, T newContext) {
		String moduleName = push.getModuleName();
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(moduleName);
		String documentName = push.getDocumentName();
		String queryName = push.getQueryName();
		String modelName = push.getModelName();
		
		if (queryName != null) {
			MetaDataQueryDefinition q = m.getMetaDataQuery(queryName);
			if (q == null) {
				q = m.getDocumentDefaultQuery(c, documentName);
			}
			m = q.getOwningModule();
			newContext.setModuleName(m.getName());
			newContext.setDocumentName(q.getDocumentName());
		}
		else if (documentName != null) {
			Document d = m.getDocument(c, documentName);
			if (modelName != null) {
				d = d.getListModel(c, modelName, false).getDrivingDocument();
			}
			else {
				push.setQueryName(documentName);
			}
			newContext.setModuleName(d.getOwningModuleName());
			newContext.setDocumentName(d.getName());
		}
		else {
			throw new MetaDataException("NavigateList must have module and one of (query, document, document & model)");
		}

		newContext.setViewType(ViewType.list);
		newContext.setUxui(push.getUxui());
		newContext.setUserAgentType(push.getUserAgentType());
		
		return newContext;
	}
	
	public T newContext(PushEditContext push, T newContext) {
		newContext.setModuleName(push.getModuleName());
		newContext.setDocumentName(push.getDocumentName());
		if (Boolean.TRUE.equals(push.getCreateView())) {
			newContext.setViewType(ViewType.create);
		}
		else {
			newContext.setViewType(ViewType.edit);
		}
		newContext.setUxui(push.getUxui());
		newContext.setUserAgentType(push.getUserAgentType());

		return newContext;
	}
	
/*
This should replace ExecutionDelegate.executeTestDataEnter but its using a Faces action presently and cant be moved from skyve-web
	public void executeTestDataEnter(TestDataEnter testDataEnter,
										T context,
										Executor executor) {
		
	}
*/
}
