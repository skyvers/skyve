package org.skyve.impl.web.faces.actions;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

import org.jboss.weld.exceptions.IllegalArgumentException;
import org.primefaces.model.charts.ChartData;
import org.primefaces.model.charts.ChartModel;
import org.primefaces.model.charts.bar.BarChartDataSet;
import org.primefaces.model.charts.bar.BarChartModel;
import org.primefaces.model.charts.donut.DonutChartDataSet;
import org.primefaces.model.charts.donut.DonutChartModel;
import org.primefaces.model.charts.hbar.HorizontalBarChartDataSet;
import org.primefaces.model.charts.hbar.HorizontalBarChartModel;
import org.primefaces.model.charts.line.LineChartDataSet;
import org.primefaces.model.charts.line.LineChartModel;
import org.primefaces.model.charts.pie.PieChartDataSet;
import org.primefaces.model.charts.pie.PieChartModel;
import org.primefaces.model.charts.polar.PolarAreaChartDataSet;
import org.primefaces.model.charts.polar.PolarAreaChartModel;
import org.primefaces.model.charts.radar.RadarChartDataSet;
import org.primefaces.model.charts.radar.RadarChartModel;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

/**
 * Create a PF chart model from a Skyve model.
 */
public class ChartAction<T extends Bean> extends FacesAction<ChartModel> {
	private FacesView<T> facesView;
	private String modelName;
	private ChartType type;
	
	public ChartAction(FacesView<T> facesView, String modelName, ChartType type) {
		this.facesView = facesView;
		this.modelName = modelName;
		this.type = type;
	}

	@Override
	public ChartModel callback() throws Exception {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("ChartAction - CHART " + modelName);

		AbstractPersistence persistence = AbstractPersistence.get();
		Bean targetBean = ActionUtil.getTargetBeanForViewAndCollectionBinding(facesView, null, null);
    	User user = persistence.getUser();
    	Customer customer = user.getCustomer();
    	Module targetModule = customer.getModule(targetBean.getBizModule());
		Document targetDocument = targetModule.getDocument(customer, targetBean.getBizDocument());
		org.skyve.metadata.view.model.chart.ChartModel<Bean> model = CORE.getRepository().getChartModel(customer, targetDocument, modelName, true);
		model.setBean(targetBean);
		org.skyve.metadata.view.model.chart.ChartData data = model.getChartData();
		
		ChartModel result = null;
		
		boolean horizontal = ChartType.horizontalBar.equals(type);
		if (ChartType.bar.equals(type) || horizontal) {
			BarChartDataSet set = horizontal ? new HorizontalBarChartDataSet() : new BarChartDataSet();
			set.setData(data.getValues());
			set.setLabel(data.getLabel());
			set.setBackgroundColor(web(data.getBackground()));
			set.setBorderColor(web(data.getBorder()));
			set.setBorderWidth(Integer.valueOf(1));
			ChartData chartData = new ChartData();
			chartData.addChartDataSet(set);
			chartData.setLabels(data.getLabels());
			BarChartModel chartModel = horizontal ? new HorizontalBarChartModel() : new BarChartModel();
			chartModel.setData(chartData);
			result = chartModel;
		}
		else if (ChartType.doughnut.equals(type)) {
			DonutChartDataSet set = new DonutChartDataSet();
			set.setData(data.getValues());
			set.setBackgroundColor(web(data.getBackgrounds()));
			set.setBorderColor(web(data.getBorders()));
			ChartData chartData = new ChartData();
			chartData.addChartDataSet(set);
			chartData.setLabels(data.getLabels());
			DonutChartModel chartModel = new DonutChartModel();
			chartModel.setData(chartData);
			result = chartModel;
		}
		else if (ChartType.line.equals(type) || ChartType.lineArea.equals(type)) {
			LineChartDataSet set = new LineChartDataSet();
			set.setData(data.getValues());
			set.setLabel(data.getLabel());
			set.setBackgroundColor(web(data.getBackground()));
			set.setBorderColor(web(data.getBorder()));
			set.setPointBackgroundColor(set.getBorderColor());
			set.setPointBorderColor(set.getBorderColor());
			set.setBorderWidth(Integer.valueOf(1));
			if (ChartType.line.equals(type)) {
				set.setFill(Boolean.FALSE);
			}
			ChartData chartData = new ChartData();
			chartData.addChartDataSet(set);
			chartData.setLabels(data.getLabels());
			LineChartModel chartModel = new LineChartModel();
			chartModel.setData(chartData);
			result = chartModel;
		}
		else if (ChartType.pie.equals(type)) {
			PieChartDataSet set = new PieChartDataSet();
			set.setData(data.getValues());
			set.setBackgroundColor(web(data.getBackgrounds()));
			set.setBorderColor(web(data.getBorders()));
			ChartData chartData = new ChartData();
			chartData.addChartDataSet(set);
			chartData.setLabels(data.getLabels());
			PieChartModel chartModel = new PieChartModel();
			chartModel.setData(chartData);
			result = chartModel;
		}
		else if (ChartType.polarArea.equals(type)) {
			PolarAreaChartDataSet set = new PolarAreaChartDataSet();
			set.setData(data.getValues());
			set.setBackgroundColor(web(data.getBackgrounds()));
			set.setBorderColor(web(data.getBorders()));
			ChartData chartData = new ChartData();
			chartData.addChartDataSet(set);
			chartData.setLabels(data.getLabels());
			PolarAreaChartModel chartModel = new PolarAreaChartModel();
			chartModel.setData(chartData);
			result = chartModel;
		}
		else if (ChartType.radar.equals(type)) {
			RadarChartDataSet set = new RadarChartDataSet();
			set.setData(data.getValues());
			set.setLabel(data.getLabel());
			set.setBackgroundColor(web(data.getBackground()));
			set.setBorderColor(web(data.getBorder()));
			set.setBorderWidth(Integer.valueOf(1));
			set.setPointBackgroundColor(set.getBorderColor());
			set.setPointBorderColor(set.getBorderColor());
			ChartData chartData = new ChartData();
			chartData.addChartDataSet(set);
			chartData.setLabels(data.getLabels());
			RadarChartModel chartModel = new RadarChartModel();
			chartModel.setData(chartData);
			result = chartModel;
		}
		else {
			throw new IllegalArgumentException("Chart Type " + type + " is not supported.");
		}
		return result;
	}
	
	private static String web(Color colour) {
		if (colour == null) {
			return null;
		}
		int alpha = colour.getAlpha();
		if (alpha > 0) {
			return new StringBuilder(16).append("rgba(").append(colour.getRed()).append(',').append(colour.getGreen()).append(',').append(colour.getBlue()).append(',').append(((alpha / 255.0) * 100) / 100.0).append(')').toString();
		}
		return new StringBuilder(16).append("rgb(").append(colour.getRed()).append(',').append(colour.getGreen()).append(',').append(colour.getBlue()).append(')').toString();
	}
	
	private static List<String> web(List<Color> colours) {
		if (colours == null) {
			return null;
		}
		List<String> result = new ArrayList<>(colours.size());
		for (Color colour : colours) {
			result.add(web(colour));
		}
		return result;
	}
}
