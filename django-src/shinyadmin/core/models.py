from django.db import models
from django.contrib.auth.models import AbstractUser


class DamsMCDAGroup(models.Model):
    """
        Groupings of users to compare/average results
    """
    name = models.CharField(max_length=255, blank=False, null=False)
    # has reverse relation to User in user field

    class Meta:
        verbose_name = 'Dams MCDA Group'
        verbose_name_plural = 'Dams MCDA Groups'

    def __str__(self):
        return "{}".format(self.name)


class DamsMCDAUser(AbstractUser):
    """
        Dam MCDA Users differ slightly from the standard user model
    """
    # groups are optional
    group = models.ForeignKey(DamsMCDAGroup, null=True, blank=True, related_name="users_in_group", on_delete=models.SET_NULL)

    class Meta:
        verbose_name = 'Dams MCDA User'
        verbose_name_plural = 'Dams MCDA Users'

    def __str__(self):
        if self.group:
            return "{0} Group:{1}".format(self.username, self.group)
        else:
            return "{0}".format(self.username)
