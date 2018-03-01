package org.skyve.impl.sail.execution;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateCalendar;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMap;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMenu;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateTree;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.ListModel;

public abstract class NavigationExecutor implements Executor {
	private User user;
	private Document drivingDocument;

	protected NavigationExecutor(User user) {
		this.user = user;
	}
	
	protected final Document getDrivingDocument() {
		return drivingDocument;
	}

	/**
	 * Do nothing but determine the driving document.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public Document execute(NavigateMenu menu) {
		return null;
	}

	/**
	 * Do nothing but determine the driving document.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public Document execute(NavigateList list) {
		Customer c = user.getCustomer();
		Module m = c.getModule(list.getModuleName());
		String documentName = list.getDocumentName();
		String queryName = list.getQueryName();
		String modelName = list.getModelName();

		Document result = null;
		if (queryName != null) {
			result = m.getDocument(c, m.getDocumentQuery(queryName).getDocumentName());
		}
		else if (documentName != null) {
			Repository r = CORE.getRepository();
			result = m.getDocument(c, documentName);

			if (modelName != null) {
				ListModel<Bean> model = r.getListModel(c, result, modelName, false);
				result = model.getDrivingDocument();
			}
		}
		return result;
	}

	/**
	 * Do nothing but determine the driving document.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public Document execute(NavigateEdit edit) {
		Customer c = user.getCustomer();
		Module m = c.getModule(edit.getModuleName());
		return m.getDocument(c, edit.getDocumentName());
	}

	/**
	 * Do nothing but determine the driving document.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public Document execute(NavigateTree tree) {
		return execute((NavigateList) tree);
	}

	/**
	 * Do nothing but determine the driving document.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public Document execute(NavigateMap map) {
		return execute((NavigateList) map);
	}

	/**
	 * Do nothing but determine the driving document.
	 * This is intended to be called by sub-classes.
	 */
	@Override
	public Document execute(NavigateCalendar calendar) {
		return execute((NavigateList) calendar);
	}
}
