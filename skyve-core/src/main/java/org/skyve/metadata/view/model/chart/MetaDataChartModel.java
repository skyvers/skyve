package org.skyve.metadata.view.model.chart;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.model.chart.ChartBuilderMetaData;
import org.skyve.impl.metadata.view.model.chart.ChartBuilderOrderMetaData;
import org.skyve.impl.metadata.view.model.chart.ChartBuilderTopMetaData;
import org.skyve.impl.metadata.view.model.chart.NoBucketMetaData;

public class MetaDataChartModel extends ChartModel<Bean> {
	private static final long serialVersionUID = -5875809858997892893L;

	private ChartBuilderMetaData builder = null;
	
	public MetaDataChartModel(ChartBuilderMetaData builder) {
		this.builder = builder;
	}
	
	@Override
	public ChartData getChartData() {
		Bucket bucket = builder.getCategoryBucket();
		if (bucket instanceof NoBucketMetaData) {
			bucket = null;
		}
		
		ChartBuilder result = new ChartBuilder().with(builder.getModuleName(), builder.getDocumentName())
									.category(builder.getCategoryBinding(), bucket)
									.value(builder.getValueBinding(), builder.getValueFunction());
		ChartBuilderTopMetaData top = builder.getTop();
		if (top != null) {
			result.top(top.getTop(), top.getBy(), top.getSort());
		}
		ChartBuilderOrderMetaData order = builder.getOrder();
		if (order != null) {
			result.orderBy(order.getBy(), order.getSort());
		}
		return result.build(builder.getLabel());
	}
}
