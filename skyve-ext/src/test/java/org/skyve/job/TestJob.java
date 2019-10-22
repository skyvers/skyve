package org.skyve.job;

import java.util.Collection;
import java.util.List;

public class TestJob extends IteratingJob<String> {

	private List<String> elements;

	@Override
	protected Collection<String> getElements() {
		return elements;
	}

	@Override
	protected void operation(String element) {
		if ("exception".equals(element)) {
			throw new RuntimeException();
		}
		System.out.println(element);
	}

	public void setElements(List<String> elements) {
		this.elements = elements;
	}
}
