let dropArea = document.getElementById('drop-area');
let imageInput = document.getElementById('image-input');
let extractForm = document.getElementById('extract-form');

const dragClasses = ['border-gray-700', 'border-4', 'bg-gray-200'];

dropArea.addEventListener('dragover', function () {
	this.classList.add(dragClasses);
});

dropArea.addEventListener('dragleave', function () {
	this.classList.remove(dragClasses);
});

dropArea.addEventListener('drop', function (e) {
	e.preventDefault();
	this.classList.remove(dragClasses);
	imageInput.files = e.dataTransfer.files;
	extractForm.submit();
});
