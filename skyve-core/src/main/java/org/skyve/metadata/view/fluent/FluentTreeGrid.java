package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Builds {@link TreeGrid} metadata by extending list-grid configuration.
 */
public class FluentTreeGrid extends FluentListGrid {
	private TreeGrid grid = null;

	/**
	 * Creates a builder backed by a new {@link TreeGrid}.
	 */
	public FluentTreeGrid() {
		this(new TreeGrid());
	}

	/**
	 * Creates a builder backed by the supplied {@link TreeGrid}.
	 *
	 * @param grid
	 *            the metadata instance to mutate
	 */
	public FluentTreeGrid(@Nonnull TreeGrid grid) {
		super(grid);
		this.grid = grid;
	}

	/**
	 * Copies inherited list-grid state and tree-root binding metadata.
	 *
	 * @param grid
	 *            the source metadata to copy
	 * @return this builder
	 */
	public @Nonnull FluentTreeGrid from(@SuppressWarnings("hiding") @Nonnull TreeGrid grid) {
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
	public @Nonnull FluentTreeGrid rootIdBinding(@Nullable String rootIdBinding) {
		grid.setRootIdBinding(rootIdBinding);
		return this;
	}

	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped tree-grid metadata
	 */
	@Override
	public @Nonnull TreeGrid get() {
		return grid;
	}
}
