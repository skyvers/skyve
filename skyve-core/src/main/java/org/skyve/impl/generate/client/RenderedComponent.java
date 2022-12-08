package org.skyve.impl.generate.client;

import java.util.ArrayList;
import java.util.List;

public class RenderedComponent {
	private String indent = null;
	private StringBuilder output = new StringBuilder(128);
	private String after = null;
	private RenderedComponent parent = null;
	private List<RenderedComponent> children = new ArrayList<>();

	public StringBuilder getOutput() {
		return output;
	}

	public RenderedComponent setAfter(String after) {
		this.after = after;
		return this;
	}

	public RenderedComponent setIndent(String indent) {
		this.indent = indent;
		return this;
	}
	public RenderedComponent getParent() {
		return parent;
	}

	public void addChild(RenderedComponent component) {
		children.add(component);
		component.parent = this;
	}

	public void addChild(int index, RenderedComponent component) {
		children.add(index, component);
		component.parent = this;
	}

	public void removeChild(RenderedComponent component) {
		children.remove(component);
		component.parent = null;
	}
	
	public RenderedComponent getChild(int index) {
		return children.get(index);
	}

	public boolean isLeaf() {
		return children.isEmpty();
	}
	
	@Override
	public String toString() {
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
				c.setIndent(indent + "\t");
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
