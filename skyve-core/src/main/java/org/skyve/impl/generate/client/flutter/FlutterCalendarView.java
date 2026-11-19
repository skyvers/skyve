package org.skyve.impl.generate.client.flutter;

import java.io.FileWriter;
import java.io.IOException;

import org.skyve.domain.Bean;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.view.model.list.ListModel;

import jakarta.annotation.Nonnull;

/**
 * Renders Flutter calendar view scaffolding from Skyve view metadata.
 */
public class FlutterCalendarView extends FlutterView {
	@SuppressWarnings("unused")
	private ListModel<Bean> model;
	@SuppressWarnings("unused")
	private MetaDataQueryDefinition query;
	
	public FlutterCalendarView(@Nonnull FlutterGenerator generator, @Nonnull String moduleName, @Nonnull String viewName) {
		super(generator, moduleName, viewName);
	}

	@Override
	protected void create(@Nonnull FileWriter fw) throws IOException {
		// TODO implement
	}
}
