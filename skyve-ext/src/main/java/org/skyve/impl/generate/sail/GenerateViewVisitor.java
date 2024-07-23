package org.skyve.impl.generate.sail;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.skyve.impl.bind.BindUtil;
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
import org.skyve.util.Binder.TargetMetaData;

public class GenerateViewVisitor extends NoOpViewVisitor {
	private List<Step> populateSteps = new ArrayList<>();
	private List<Step> actionSteps = new ArrayList<>();
	private boolean hasSave = false;
	private boolean hasOk = false;
	private boolean hasCancel = false;
	private boolean hasDelete = false;
	// list of module.document.view.binding visit in data grids to stop infinite recursion
	private Set<String> breadcrumbs;
	
	protected GenerateViewVisitor(Customer customer, 
										Module module,
										Document document,
										String uxui) {
		super((CustomerImpl) customer, 
				(ModuleImpl) module,
				(DocumentImpl) document,
				determineView(customer, document, uxui),
				uxui);
		breadcrumbs = new TreeSet<>();
		populateSteps.add(new TestDataEnter());
	}
	
	private GenerateViewVisitor(Customer customer, 
									Module module,
									Document document,
									String uxui,
									Set<String> breadcrumbs) {
		super((CustomerImpl) customer, 
				(ModuleImpl) module,
				(DocumentImpl) document,
				determineView(customer, document, uxui),
				uxui);
		this.breadcrumbs = breadcrumbs;
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
		select.setTabPath(tab.getLocalisedTitle());
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

		// Do nothing if we've visited this grid on this view before
		String breadcrumb = String.format("%s.%s.%s.%s", module.getName(), document.getName(), view.getName(), binding);
		if (! breadcrumbs.add(breadcrumb)) {
			return;
		}
		
		DataGridNew nu = new DataGridNew();
		nu.setBinding(binding);
		populateSteps.add(nu);
		
		TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
		Relation relation = (Relation) target.getAttribute();
		if (relation != null) { // should always be
			Document gridDocument = module.getDocument(customer, relation.getDocumentName());
			Module gridModule = customer.getModule(gridDocument.getOwningModuleName());
			GenerateViewVisitor gridVisitor = new GenerateViewVisitor(customer, gridModule, gridDocument, currentUxUi, breadcrumbs);
			gridVisitor.visit();
			populateSteps.addAll(gridVisitor.populateSteps);

			populateSteps.add(new ZoomOut());
		}
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
			addCustomAction(action);
		}
	}
	
	@Override
	public void visitButton(Button button, boolean parentVisible, boolean parentEnabled) {
		ActionImpl action = (ActionImpl) view.getAction(button.getActionName());
		addCustomAction(action);
	}
	
	private void addCustomAction(ActionImpl action) {
		Action step = new Action();
		step.setActionName(action.getName());
		if (action.getConfirmationText() != null) {
			step.setConfirm(Boolean.TRUE);
		}
		actionSteps.add(step);
	}
	
	List<Step> getPopulateSteps() {
		return populateSteps;
	}
	
	List<Step> getActionSteps() {
		return actionSteps;
	}
	
	boolean getHasSave() {
		return hasSave && document.isPersistable();
	}
	
	boolean getHasOk() {
		return hasOk && document.isPersistable();
	}
	
	boolean getHasCancel() {
		return hasCancel;
	}
	
	boolean getHasDelete() {
		return hasDelete && document.isPersistable();
	}
}
