package org.skyve.impl.sail.execution;

import java.util.Stack;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.web.UserAgentType;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateCalendar;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateLink;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMap;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMenu;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateTree;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.list.ListModel;

public abstract class ContextualExecutor<T extends AutomationContext> implements Executor {
	private String defaultUxUi = null;
	private UserAgentType defaultUserAgentType = null;
	
	private Stack<T> contextStack = new Stack<>();

	protected final void push(T context) {
		if (context.getUxui() == null) {
			context.setUxui(defaultUxUi);
		}
		if (context.getUserAgentType() == null) {
			context.setUserAgentType(defaultUserAgentType);
		}
		contextStack.push(context);
	}

	protected final T pop() {
		return contextStack.pop();
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
	public void execute(Automation automation) {
		defaultUxUi = automation.getUxui();
		defaultUserAgentType = automation.getUserAgentType();
	}
	
	/**
	 * Do nothing but populate the automation context.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public void execute(NavigateMenu menu) {
		// TODO implement
	}

	/**
	 * Do nothing but populate the automation context.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public void execute(NavigateList list) {
		T context = peek();
		context.setViewType(ViewType.list);
		
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(list.getModuleName());
		String documentName = list.getDocumentName();
		String queryName = list.getQueryName();
		String modelName = list.getModelName();

		Document drivingDocument = null;
		if (queryName != null) {
			drivingDocument = m.getDocument(c, m.getDocumentQuery(queryName).getDocumentName());
		}
		else if (documentName != null) {
			Repository r = CORE.getRepository();
			drivingDocument = m.getDocument(c, documentName);

			if (modelName != null) {
				ListModel<Bean> model = r.getListModel(c, drivingDocument, modelName, false);
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
	public void execute(NavigateEdit edit) {
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
	public void execute(NavigateTree tree) {
		execute((NavigateList) tree);
	}

	/**
	 * Do nothing but populate the automation context.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public void execute(NavigateMap map) {
		execute((NavigateList) map);
	}

	/**
	 * Do nothing but populate the automation context.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public void execute(NavigateCalendar calendar) {
		execute((NavigateList) calendar);
	}
	
	/**
	 * Do nothing but clear the automation context.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public void execute(NavigateLink link) {
		T context = peek();
		context.setDocumentName(null);
		context.setViewType(null);
	}
}
