package org.skyve.impl.generate.client;

import java.util.ArrayList;
import java.util.List;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Value object holding rendered client component output fragments.
 *
 * <p>Used to pass generated markup/code and related metadata between
 * renderer stages.
 */
public class RenderedComponent {
	private String indentation = "\t";
	private String indent = null;
	private StringBuilder output = new StringBuilder(128);
	private String after = null;
	private RenderedComponent parent = null;
	private List<RenderedComponent> children = new ArrayList<>();

	public RenderedComponent() {
		// nothing to see here
	}
	
	public RenderedComponent(@Nonnull String indentation) {
		this.indentation = indentation;
	}
	
	public @Nonnull StringBuilder getOutput() {
		return output;
	}

	public @Nonnull RenderedComponent setAfter(@Nullable String after) {
		this.after = after;
		return this;
	}

	public @Nonnull RenderedComponent setIndent(@Nullable String indent) {
		this.indent = indent;
		return this;
	}
	
	public @Nullable RenderedComponent getParent() {
		return parent;
	}

	public void addChild(@Nonnull RenderedComponent component) {
		children.add(component);
		component.parent = this;
	}

	public void addChild(int index, @Nonnull RenderedComponent component) {
		children.add(index, component);
		component.parent = this;
	}

	public void removeChild(@Nonnull RenderedComponent component) {
		children.remove(component);
		component.parent = null;
	}
	
	public @Nonnull RenderedComponent getChild(int index) {
		return children.get(index);
	}

	public boolean isLeaf() {
		return children.isEmpty();
	}
	
	@Override
	public @Nonnull String toString() {
		StringBuilder result = new StringBuilder(2048);
		if (indent != null) {
			result.append(indent);
		}
		result.append(output);
		if (indent != null) {
			result.append('\n');
		}
		for (RenderedComponent c : children) {
			if (indent != null) {
				c.setIndent(indent + indentation);
			}
			result.append(c.toString());
		}
		if (after != null) {
			if (indent != null) {
				result.append(indent);
			}
			result.append(after);
			if (indent != null) {
				result.append('\n');
			}
		}
		return result.toString();
	}
}
