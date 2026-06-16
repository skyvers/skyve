(function() {
	"use strict";

	var suppressChooseClick = false;

	function chooseVideo() {
		if (suppressChooseClick) {
			suppressChooseClick = false;
			return;
		}
		$("#_af").val("generic");
		$("#upload_input").removeAttr("capture");
	}

	window.captureVideo = function() {
		suppressChooseClick = true;
		$("#_af").val("video");
		$("#upload_input").attr("capture", "camcorder");
		$("#upload_input").trigger("click");
		setTimeout(function() {
			suppressChooseClick = false;
		}, 0);
	};

	$(document).ready(function() {
		$("#video").appendTo($(".ui-fileupload .ui-fileupload-buttonbar")).show();
		$("#upload .ui-fileupload-choose").on("click", chooseVideo);
	});
}());
