package org.skyve.impl.generate.sail;

import java.util.List;

import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.NoOpViewVisitor;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.interaction.TabSelect;
import org.skyve.metadata.sail.language.step.interaction.TestDataEnter;
import org.skyve.metadata.sail.language.step.interaction.actions.ZoomOut;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridNew;

public class GenerateViewVisitor extends NoOpViewVisitor {
	private List<Step> steps;
	
	protected GenerateViewVisitor(CustomerImpl customer, 
										ModuleImpl module,
										DocumentImpl document,
										ViewImpl view,
										List<Step> steps) {
		super(customer, module, document, view);
		this.steps = steps;
	}

	@Override
	public void visitTab(Tab tab, boolean parentVisible, boolean parentEnabled) {
		TabSelect select = new TabSelect();
		select.setTabPath(tab.getTitle());
		steps.add(select);
	}
	
	@Override
	public void visitDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		DataGridNew nu = new DataGridNew();
		nu.setBinding(grid.getBinding());
		steps.add(nu);
		
		// TODO need to recurse here with another visitor
		steps.add(new TestDataEnter());
		steps.add(new ZoomOut());
	}
}
