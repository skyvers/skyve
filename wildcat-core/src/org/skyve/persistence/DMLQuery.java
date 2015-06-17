package org.skyve.persistence;

import org.skyve.domain.messages.DomainException;

public interface DMLQuery {
	int execute() throws DomainException;
}
