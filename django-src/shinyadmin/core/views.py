from django.shortcuts import render, redirect
from django.contrib.auth import authenticate, login
from django.contrib.auth.forms import AuthenticationForm

# Create your views here.

def landing_page(request):
    if request.user.is_authenticated:
        return redirect('/dams_mcda/')
    else:
        return redirect('/login/')


def login_view(request):
    if request.method == "POST":
        username = request.POST['username']
        password = request.POST['password']

        user = authenticate(request, username=username, password=password)

        if user is not None:
            login(request, user)
            return redirect('/')
        else:
            form = AuthenticationForm(request.POST)
            return render(request, "login.html", {'form': form, 'errors': 'invalid login credentials, please try again'})
    else:
        form = AuthenticationForm()
        return render(request, "login.html", locals())

