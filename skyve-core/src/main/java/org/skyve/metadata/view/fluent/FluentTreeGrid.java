package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;

/**
 * Builds {@link TreeGrid} metadata by extending list-grid configuration.
 */
public class FluentTreeGrid extends FluentListGrid {
	private TreeGrid grid = null;

	/**
	 * Creates a builder backed by a new {@link TreeGrid}.
	 */
	public FluentTreeGrid() {
		grid = new TreeGrid();
	}

	/**
	 * Creates a builder backed by the supplied {@link TreeGrid}.
	 *
	 * @param grid
	 *            the metadata instance to mutate
	 */
	public FluentTreeGrid(TreeGrid grid) {
		this.grid = grid;
	}

	/**
	 * Copies inherited list-grid state and tree-root binding metadata.
	 *
	 * @param grid
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentTreeGrid from(@SuppressWarnings("hiding") TreeGrid grid) {
		super.from(grid);
		rootIdBinding(grid.getRootIdBinding());
		return this;
	}

	/**
	 * Sets the binding used to resolve the root node identifier.
	 *
	 * @param rootIdBinding
	 *            the root-id binding expression
	 * @return this builder
	 */
	public FluentTreeGrid rootIdBinding(String rootIdBinding) {
		grid.setRootIdBinding(rootIdBinding);
		return this;
	}

	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped tree-grid metadata
	 */
	@Override
	public TreeGrid get() {
		return grid;
	}
}
