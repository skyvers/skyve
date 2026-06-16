(function() {
	"use strict";

	var cropper = null;
	var file = null;
	var fileName = null;
	var fileFormat = "png";
	var cameraMode = false;
	var autoEditAfterFileSelection = false;
	var suppressChooseClick = false;
	var maximumImageDimension = 4096;
	var minimumImageDimension = 320;
	var currentImageUrl = null;
	var resizeTimer = null;

	function sizeCropperImage(image) {
		var frame = document.getElementById("cropperFrame");
		var frameWidth = frame.clientWidth;
		var frameHeight = frame.clientHeight;
		if (! (frameWidth > 0) || ! (frameHeight > 0)) {
			return;
		}
		if ((frameWidth / frameHeight) > (image.naturalWidth / image.naturalHeight)) {
			image.style.width = "auto";
			image.style.height = frameHeight + "px";
		}
		else {
			image.style.width = frameWidth + "px";
			image.style.height = "auto";
		}
	}

	function initialiseCropper(imageUrl) {
		currentImageUrl = imageUrl;
		var image = document.getElementById("cropperImage");
		if (cropper != null) {
			cropper.destroy();
			cropper = null;
		}
		image.onload = function() {
			window.requestAnimationFrame(function() {
				sizeCropperImage(image);
				cropper = new Cropper(image, {
					viewMode: 1,
					dragMode: "move",
					autoCropArea: 1,
					background: false,
					checkOrientation: true,
					responsive: true,
					ready: function() {
						cropper.setCropBoxData(cropper.getCanvasData());
					}
				});
			});
		};
		image.src = "";
		image.src = imageUrl;
	}

	function canvasForMimeType(canvas, mimeType) {
		if (mimeType !== "image/jpeg") {
			return canvas;
		}
		var targetCanvas = document.createElement("canvas");
		targetCanvas.width = canvas.width;
		targetCanvas.height = canvas.height;
		var context = targetCanvas.getContext("2d");
		context.fillStyle = "#fff";
		context.fillRect(0, 0, targetCanvas.width, targetCanvas.height);
		context.drawImage(canvas, 0, 0);
		return targetCanvas;
	}

	function canvasToBlob(canvas, mimeType, quality) {
		var outputCanvas = canvasForMimeType(canvas, mimeType);
		return new Promise(function(resolve, reject) {
			outputCanvas.toBlob(function(blob) {
				if (blob == null) {
					reject(new Error("Could not encode cropped image"));
				}
				else {
					resolve(blob);
				}
			}, mimeType, quality);
		});
	}

	function createScaledCanvas(sourceCanvas, width, height) {
		var targetCanvas = document.createElement("canvas");
		targetCanvas.width = width;
		targetCanvas.height = height;
		var context = targetCanvas.getContext("2d");
		context.imageSmoothingEnabled = true;
		context.imageSmoothingQuality = "high";
		context.drawImage(sourceCanvas, 0, 0, width, height);
		return targetCanvas;
	}

	function scaleCanvas(sourceCanvas, scale) {
		var width = Math.max(1, Math.round(sourceCanvas.width * scale));
		var height = Math.max(1, Math.round(sourceCanvas.height * scale));
		return createScaledCanvas(sourceCanvas, width, height);
	}

	function scaleCanvasToMaximumDimension(sourceCanvas, maximumDimension) {
		var dimension = Math.max(sourceCanvas.width, sourceCanvas.height);
		if (! (dimension > maximumDimension)) {
			return sourceCanvas;
		}
		return scaleCanvas(sourceCanvas, maximumDimension / dimension);
	}

	function fileExtension(format) {
		return format === "jpeg" ? "jpg" : format;
	}

	function croppedFileName(format) {
		var baseName = file.name.replace(/\.[^/.]+$/, "");
		return baseName + ".cropped." + fileExtension(format);
	}

	function formatBytes(bytes) {
		if (bytes >= 1048576) {
			return (bytes / 1048576).toFixed(1) + "MB";
		}
		return Math.ceil(bytes / 1024) + "KB";
	}

	async function encodeCanvasForUpload(croppedCanvas) {
		var maximumUploadSizeInBytes = window.skyveMaximumUploadSizeInBytes || 0;
		var outputFormat = cameraMode ? "jpeg" : fileFormat;
		var mimeType = "image/" + outputFormat;
		var canvas = scaleCanvasToMaximumDimension(croppedCanvas, maximumImageDimension);
		var quality = outputFormat === "jpeg" ? 0.92 : 1;
		var blob = await canvasToBlob(canvas, mimeType, quality);

		if (blob.size > maximumUploadSizeInBytes) {
			outputFormat = "jpeg";
			mimeType = "image/jpeg";
			quality = 0.92;
			blob = await canvasToBlob(canvas, mimeType, quality);
		}

		while ((blob.size > maximumUploadSizeInBytes) && (quality > 0.55)) {
			quality = Math.max(0.55, quality - 0.07);
			blob = await canvasToBlob(canvas, mimeType, quality);
		}

		while ((blob.size > maximumUploadSizeInBytes) &&
				(Math.max(canvas.width, canvas.height) > minimumImageDimension)) {
			canvas = scaleCanvas(canvas, 0.85);
			quality = 0.82;
			blob = await canvasToBlob(canvas, mimeType, quality);
		}

		fileName = croppedFileName(outputFormat);
		return blob;
	}

	function uploadCropped(blob) {
		var formData = new FormData(document.getElementById("form"));
		formData.append("jakarta.faces.partial.ajax", "true");
		formData.append("jakarta.faces.partial.execute", "upload");
		formData.append("jakarta.faces.source", "upload");
		formData.append("jakarta.faces.partial.render", "messages");
		formData.append("upload", blob, fileName);

		$.ajax({
			type: "POST",
			url: window.location.href,
			data: formData,
			processData: false,
			contentType: false
		}).done(function(data, status, xhr) {
			PrimeFaces.ajax.Response.handle(data, status, xhr);
		});
	}

	window.cropAndUpload = async function() {
		if (cropper == null) {
			return;
		}
		try {
			var croppedCanvas = cropper.getCroppedCanvas({
				imageSmoothingEnabled: true,
				imageSmoothingQuality: "high"
			});
			var blob = await encodeCanvasForUpload(croppedCanvas);
			if (blob.size > window.skyveMaximumUploadSizeInBytes) {
				alert("The cropped image is still too large to upload. Maximum size is " +
						formatBytes(window.skyveMaximumUploadSizeInBytes) + ". Encoded size is " + formatBytes(blob.size) + ".");
				return;
			}
			uploadCropped(blob);
		}
		catch (e) {
			alert(e.message || "Could not process cropped image");
		}
	};

	window.editImage = function() {
		fileFormat = "png";
		if (file.type === "image/jpeg") {
			fileFormat = "jpeg";
		}
		fileName = croppedFileName(fileFormat);
		$("#cropperBar").css({display: "block"});
		$("#cropperContainer").css({display: "flex"});
		var reader = new FileReader();
		reader.onload = function(evt) {
			initialiseCropper(evt.target.result);
		};
		reader.readAsDataURL(file);
		$("#upload").css({display: "none"});
	};

	function chooseImage() {
		if (suppressChooseClick) {
			suppressChooseClick = false;
			return;
		}
		cameraMode = false;
		autoEditAfterFileSelection = false;
		$("#_af").val("generic");
		$("#upload_input").removeAttr("capture");
	}

	window.captureImage = function() {
		cameraMode = true;
		autoEditAfterFileSelection = true;
		suppressChooseClick = true;
		$("#_af").val("camera");
		$("#upload_input").attr("capture", "environment");
		$("#upload_input").trigger("click");
		setTimeout(function() {
			suppressChooseClick = false;
		}, 0);
	};

	window.rotateImage = function(degrees) {
		if (cropper != null) {
			cropper.rotate(degrees);
		}
	};

	function clearSelectedImage() {
		file = null;
		cameraMode = false;
		autoEditAfterFileSelection = false;
		$("#_af").val("generic");
		$("#upload_input").removeAttr("capture");
		$("#camera").show();
		var editButton = PF("editButton");
		if (editButton) {
			editButton.disable();
		}
	}

	$(document).ready(function() {
		if (! document.getElementById("cropperContainer")) {
			return;
		}
		$("#camera").appendTo($(".ui-fileupload .ui-fileupload-buttonbar")).show();
		$("#edit").appendTo($(".ui-fileupload .ui-fileupload-buttonbar")).show();
		$("#upload .ui-fileupload-choose").on("click", chooseImage);

		$("#upload_input").on("change", function() {
			if (this.files && this.files[0]) {
				file = this.files[0];
				if (autoEditAfterFileSelection) {
					autoEditAfterFileSelection = false;
					window.editImage();
				}
				else if (PF("editButton")) {
					PF("editButton").enable();
					$("#camera").hide();
				}

				setTimeout(function() {
					$(".ui-fileupload-cancel").on("click", function() {
						clearSelectedImage();
					});
				}, 500);
			}
		});

		$(window).on("resize", function() {
			if ((currentImageUrl == null) || ($("#cropperContainer").css("display") === "none")) {
				return;
			}
			clearTimeout(resizeTimer);
			resizeTimer = setTimeout(function() {
				initialiseCropper(currentImageUrl);
			}, 200);
		});
	});
}());
