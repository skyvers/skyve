package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.AbstractInverse;
import org.skyve.impl.metadata.model.document.InverseMany;

public class FluentInverseMany extends FluentInverse<FluentInverseMany> {
	private InverseMany inverse = new InverseMany();
	
	public FluentInverseMany() {
		// nothing to see
	}

	public FluentInverseMany(InverseMany inverse) {
		super(inverse);
	}
	
	@Override
	public AbstractInverse get() {
		return inverse;
	}
}
