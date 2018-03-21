package org.skyve.impl.generate.sail;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.NoOpViewVisitor;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.interaction.TabSelect;
import org.skyve.metadata.sail.language.step.interaction.TestDataEnter;
import org.skyve.metadata.sail.language.step.interaction.actions.Action;
import org.skyve.metadata.sail.language.step.interaction.actions.ZoomOut;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridNew;
import org.skyve.metadata.view.View.ViewType;

public class GenerateViewVisitor extends NoOpViewVisitor {
	private String uxui;
	private List<Step> populateSteps = new ArrayList<>();
	private List<Step> actionSteps = new ArrayList<>();
	private boolean hasSave = false;
	private boolean hasOk = false;
	private boolean hasCancel = false;
	private boolean hasDelete = false;
	
	protected GenerateViewVisitor(Customer customer, 
										Module module,
										Document document,
										String uxui) {
		super((CustomerImpl) customer, 
				(ModuleImpl) module,
				(DocumentImpl) document,
				determineView(customer, document, uxui));
		this.uxui = uxui;
		populateSteps.add(new TestDataEnter());
	}

	private static ViewImpl determineView(Customer c, Document d, String uxui) {
		ViewImpl createView = (ViewImpl) d.getView(uxui, c, ViewType.create.toString());
		ViewImpl editView = (ViewImpl) d.getView(uxui, c, ViewType.edit.toString());
		
		return (createView != editView) ? createView : editView;
	}

	@Override
	public void visitTab(Tab tab, boolean parentVisible, boolean parentEnabled) {
		TabSelect select = new TabSelect();
		select.setTabPath(tab.getTitle());
		populateSteps.add(select);
		actionSteps.add(select);
	}
	
	@Override
	public void visitDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		// Do nothing if we can't add a row
		if (Boolean.FALSE.equals(grid.getEditable()) || Boolean.FALSE.equals(grid.getShowAdd())) {
			return;
		}

		String binding = grid.getBinding();

		DataGridNew nu = new DataGridNew();
		nu.setBinding(binding);
		populateSteps.add(nu);
		
		Relation relation = (Relation) document.getAttribute(binding);
		Document gridDocument = module.getDocument(customer, relation.getDocumentName());
		Module gridModule = customer.getModule(gridDocument.getOwningModuleName());
		GenerateViewVisitor gridVisitor = new GenerateViewVisitor(customer, gridModule, gridDocument, uxui);
		gridVisitor.visit();
		populateSteps.addAll(gridVisitor.populateSteps);

		populateSteps.add(new ZoomOut());
	}
	
	@Override
	public void visitSaveAction(ActionImpl action) {
		hasSave = true;
	}
	
	@Override
	public void visitOKAction(ActionImpl action) {
		hasOk = true;
	}

	@Override
	public void visitCancelAction(ActionImpl action) {
		hasCancel = true;
	}

	@Override
	public void visitDeleteAction(ActionImpl action) {
		hasDelete = true;
	}
	
	@Override
	public void visitCustomAction(ActionImpl action) {
		if (! Boolean.FALSE.equals(action.getInActionPanel())) {
			Action step = new Action();
			step.setActionName(action.getName());
			if (action.getConfirmationText() != null) {
				step.setConfirm(Boolean.TRUE);
			}
			actionSteps.add(step);
		}
	}
	
	@Override
	public void visitButton(Button button, boolean parentVisible, boolean parentEnabled) {
		ActionImpl action = (ActionImpl) view.getAction(button.getActionName());
		visitCustomAction(action);
	}
	
	List<Step> getPopulateSteps() {
		return populateSteps;
	}
	
	List<Step> getActionSteps() {
		return actionSteps;
	}
	
	boolean getHasSave() {
		return hasSave && (document.getPersistent() != null);
	}
	
	boolean getHasOk() {
		return hasOk&& (document.getPersistent() != null);
	}
	
	boolean getHasCancel() {
		return hasCancel;
	}
	
	boolean getHasDelete() {
		return hasDelete&& (document.getPersistent() != null);
	}
}
